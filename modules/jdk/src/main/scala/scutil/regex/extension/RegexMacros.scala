package scutil.regex.extension

import java.util.regex.PatternSyntaxException

import scala.quoted.*
import scala.util.matching.Regex

private object RegexMacros {
	def re(context:Expr[StringContext])(using Quotes):Expr[Regex]	= {
		import quotes.reflect.*

		val literal	=
			context.valueOrAbort.parts match {
				case Seq(it)	=> it
				case _			=> report.errorAndAbort("interpolation is not supported")
			}

		try {
			literal.r
		}
		catch { case e:PatternSyntaxException =>
			report.errorAndAbort(s"invalid regex literal ${literal} at index ${e.getIndex.toString}: ${e.getDescription.toString}")
		}

		val text	= Expr(literal)

		'{ $text.r }
	}
}