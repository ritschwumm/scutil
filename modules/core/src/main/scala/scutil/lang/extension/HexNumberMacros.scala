package scutil.lang.extension

import scala.quoted.*

import scutil.codec.Hex

private object HexNumberMacros {
	def byte	(context:Expr[StringContext])(using Quotes):Expr[Byte]	= impl(context, 1, _.byteValue)
	def short	(context:Expr[StringContext])(using Quotes):Expr[Short]	= impl(context, 2, _.shortValue)
	def int		(context:Expr[StringContext])(using Quotes):Expr[Int]	= impl(context, 4, _.intValue)
	def long	(context:Expr[StringContext])(using Quotes):Expr[Long]	= impl(context, 8,_.longValue)

	private def impl[T:ToExpr](context:Expr[StringContext], byteCount:Int, func:BigInt=>T)(using Quotes):Expr[T]	= {
		import quotes.reflect.*

		val literal	=
			context.valueOrAbort.parts match {
				case Seq(it)	=> it
				case _			=> report.errorAndAbort("interpolation is not supported")
			}

		val decoded	=
			Hex.decodeByteString(literal).getOrElse(report.errorAndAbort(s"cannot decode hex number ${literal}"))

		if (decoded.size != byteCount) {
			report.errorAndAbort(s"unexpected hex number length in ${literal}: expected ${byteCount.toString}, found ${decoded.size.toString}")
		}

		val value	= func(BigInt(decoded.unsafeValue))
		Expr(value)
	}
}
