package scutil.lang.extension

import scala.quoted.*

import scutil.lang.GenericLiteral
import scutil.codec.Hex

object HexNumberLiterals {
	extension (inline peer:StringContext) {
		/** provide string interpolator for hex bytes */
		inline def byte(inline args:Any*):Byte		= ${ byteImpl('peer) }

		/** provide string interpolator for hex bytes */
		inline def short(inline args:Any*):Short	= ${ shortImpl('peer) }

		/** provide string interpolator for hex bytes */
		inline def int(inline args:Any*):Int		= ${ intImpl('peer) }

		/** provide string interpolator for hex bytes */
		inline def long(inline args:Any*):Long		= ${ longImpl('peer) }
	}

	private def byteImpl	(context:Expr[StringContext])(using Quotes):Expr[Byte]	= GenericLiteral.build(context)(parser(1, _.byteValue))
	private def shortImpl	(context:Expr[StringContext])(using Quotes):Expr[Short]	= GenericLiteral.build(context)(parser(2, _.shortValue))
	private def intImpl		(context:Expr[StringContext])(using Quotes):Expr[Int]	= GenericLiteral.build(context)(parser(4, _.intValue))
	private def longImpl	(context:Expr[StringContext])(using Quotes):Expr[Long]	= GenericLiteral.build(context)(parser(8, _.longValue))

	private def parser[T](byteCount:Int, convert:BigInt=>T)(literal:String):Either[String,T]	=
		Hex.decodeByteString(literal) match {
			case Some(x) if x.size == byteCount	=> Right(convert(BigInt(x.unsafeValue)))
			case Some(x)						=> Left("unexpected HexNumber literal length")
			case None							=> Left("not a valid HexNumber literal")
		}
}
