package scutil.color.extension

import scala.quoted.*

import scutil.lang.GenericLiteral
import scutil.color.*

object HexColorLiterals {
	/** provide string interpolators for web-style hex colors */
	extension (inline peer:StringContext) {
		inline def rgb(inline args:Any*):RGB	= ${ rgbImpl('peer) }
		inline def rgba(inline args:Any*):RGBA	= ${ rgbaImpl('peer) }
	}

	private def rgbImpl(context:Expr[StringContext])(using Quotes):Expr[RGB]	=
		GenericLiteral.build(context) { literal =>
			RGB.parseHex(literal).toRight("not a valid RBG literal")
		}

	private def rgbaImpl(context:Expr[StringContext])(using Quotes):Expr[RGBA]	=
		GenericLiteral.build(context) { literal =>
			RGBA.parseHex(literal).toRight("not a valid RGBA literal")
		}

	private given ToExpr[RGBA]	with {
		def apply(it:RGBA)(using Quotes):Expr[RGBA] =
			'{
				RGBA(
					${Expr(it.rgb)},
					${Expr(it.alpha)},
				)
			}
	}

	private given ToExpr[RGB]	with {
		def apply(it:RGB)(using Quotes):Expr[RGB] =
			'{
				RGB(
					${Expr(it.r)},
					${Expr(it.g)},
					${Expr(it.b)},
				)
			}
	}

	private given ToExpr[Alpha]	with {
		def apply(it:Alpha)(using Quotes):Expr[Alpha] =
			'{
				Alpha(
					${Expr(it.a)},
				)
			}
	}
}
