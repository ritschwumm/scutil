package scutil.number.extension

import java.math.{ BigInteger => JBigInteger }

import scala.quoted.*

import scutil.number.BigRational

private object BigRationalMacros {
	def br(context:Expr[StringContext])(using Quotes):Expr[BigRational]	= {
		import quotes.reflect.*

		val literal	=
			context.valueOrAbort.parts match {
				case Seq(it)	=> it
				case _			=> report.errorAndAbort("interpolation is not supported")
			}

		val decoded	=
			BigRational.parse(literal).getOrElse(report.errorAndAbort(s"cannot decode big rational number ${literal}"))

		Expr(decoded)
	}

	private given BigRationalToExpr:ToExpr[BigRational]	= new ToExpr[BigRational] {
		def apply(it:BigRational)(using Quotes):Expr[BigRational] = {
			val num	= Expr(it.numerator)
			val den	= Expr(it.denominator)
			'{ BigRational.unsafeCreate($num, $den) }
		}
	}

	private given JBigIntegerToExpr:ToExpr[JBigInteger] = new ToExpr[JBigInteger] {
		def apply(it:JBigInteger)(using Quotes):Expr[JBigInteger] = {
			val value	= Expr(it.toByteArray)
			'{ new JBigInteger($value) }
		}
	}
}
