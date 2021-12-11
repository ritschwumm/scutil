package scutil.lang

import scala.quoted.*

object GenericLiteral {
	def build[T:ToExpr](context:Expr[StringContext])(parse:String=>Either[String,T])(using Quotes):Expr[T]	= {
		import quotes.reflect.*

		context.valueOrAbort.parts match {
			case Seq(literal)	=>
				parse(literal) match {
					case Right(value)	=> Expr(value)
					case Left(e)		=> report.errorAndAbort(e)
				}
			case _	=> report.errorAndAbort("interpolated values are not supported")
		}
	}
}
