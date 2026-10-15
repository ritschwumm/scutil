package scutil.number.extension

import java.math.{ BigInteger as JBigInteger }

import scala.annotation.nowarn
import scala.quoted.*

import scutil.lang.GenericLiteral
import scutil.number.BigRational

@nowarn("msg=unused explicit parameter in extension method")
object BigRationalLiterals {
	extension (inline peer:StringContext) {
		inline def br(inline args:Any*):BigRational	= ${ brImpl('peer) }
	}

	private def brImpl(context:Expr[StringContext])(using Quotes):Expr[BigRational]	=
		GenericLiteral.build(context) { literal =>
			BigRational.parse(literal) match {
				case Some(x)	=> Right(x)
				case None		=> Left("not a valid BigRational literal")
			}
		}

	private given ToExpr[BigRational] {
		def apply(it:BigRational)(using Quotes):Expr[BigRational] =
			'{
				BigRational.unsafeCreate(
					${Expr(it.numerator)},
					${Expr(it.denominator)}
				)
			}
	}

	private given ToExpr[JBigInteger] {
		def apply(it:JBigInteger)(using Quotes):Expr[JBigInteger] =
			'{
				new JBigInteger(
					${Expr(it.toByteArray)}
				)
			}
	}
}
