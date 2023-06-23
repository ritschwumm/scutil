package scutil.lang.extension

import scala.quoted.*

import scutil.lang.GenericLiteral
import scutil.lang.ByteString
import scutil.codec.Hex

object ByteStringLiteral {
	extension (inline peer:StringContext) {
		/** provide string interpolator for hex bytes */
		inline def bytes(inline args:Any*):ByteString	= ${ bytesImpl('peer) }
	}

	private val validRe	= "^(\\s*[0-9a-fA-F]{2})*\\s*$".r

	private def bytesImpl(context:Expr[StringContext])(using Quotes):Expr[ByteString]	=
		GenericLiteral.build(context){ literal =>
			if (validRe.matches(literal)) {
				val deblanked	= literal.replaceAll("\\s", "")
				Hex.decodeByteString(deblanked).toRight("not a valid ByteString literal")
			}
			else Left("invalid whitespace in ByteString literal")
		}

	private given ToExpr[ByteString]	with {
		def apply(it:ByteString)(using Quotes):Expr[ByteString] =
			'{
				ByteString.unsafeFromArray(
					${Expr(it.unsafeValue)}
				)
			}
	}
}
