package scutil.regex.extension

import java.util.regex.PatternSyntaxException

import scala.quoted.*
import scala.util.matching.Regex

import scutil.lang.GenericLiteral

object RegexLiterals {
	extension (inline peer:StringContext) {
		inline def re(inline args:Any*):Regex	= ${ reImpl('peer) }
	}

	private def reImpl(context:Expr[StringContext])(using Quotes):Expr[Regex]	=
		GenericLiteral.build(context) { literal =>
			try {
				Right(new Regex(literal))
			}
			catch { case e:PatternSyntaxException =>
				Left(e.getDescription.toString)
			}
		}

	// we have to pass the string here: the Regex constructor calls Pattern.compile, but we cannot create a Regex from a Pattern
	private given ToExpr[Regex]	with {
		def apply(it:Regex)(using Quotes):Expr[Regex] =
			'{
				new Regex(
					${Expr(it.regex)}
				)
			}
	}
}
