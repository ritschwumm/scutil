package scutil.color.extension

import scala.quoted.*

import scutil.color._

private object HexColorMacros {
	def rgb(context:Expr[StringContext])(using Quotes):Expr[RGB]	= {
		import quotes.reflect.*

		val literal	=
			context.valueOrAbort.parts match {
				case Seq(it)	=> it
				case _			=> report.errorAndAbort("interpolation is not supported")
			}

		val decoded	=
			RGB.parseHex(literal).getOrElse(report.errorAndAbort(s"cannot decode rgb value ${literal}"))

		val r	= Expr(decoded.r)
		val g	= Expr(decoded.g)
		val b	= Expr(decoded.b)

		'{ RGB($r, $g, $b) }
	}

	def rgba(context:Expr[StringContext])(using Quotes):Expr[RGBA]	= {
		import quotes.reflect.*

		val literal	=
			context.valueOrAbort.parts match {
				case Seq(it)	=> it
				case _			=> report.errorAndAbort("interpolation is not supported")
			}

		val decoded	=
			RGBA.parseHex(literal).getOrElse(report.errorAndAbort(s"cannot decode rgba value ${literal}"))

		val r	= Expr(decoded.r)
		val g	= Expr(decoded.g)
		val b	= Expr(decoded.b)
		val a	= Expr(decoded.a)

		'{ RGBA(RGB($r, $g, $b), Alpha($a)) }
	}
}
