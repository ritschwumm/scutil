package scutil.lang

import scala.quoted.*

object Companion {
	/** provides the companion object for some type */
	inline transparent def of[T]:Any = ${ ofImpl[T] }

	def ofImpl[T](using quotes:Quotes, tpe:Type[T]):Expr[Any]	= {
		import quotes.reflect.*
		Ref(TypeTree.of[T].symbol.companionModule).asExpr
	}
}
