package scutil.lang

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object PrismGen {
	def apply[S,T <: S]:AnyRef	= macro PrismGen.compile[S,T]
}

private final class PrismGen(val c:Context) {
	import c.universe._

	def compile[S:c.WeakTypeTag,T:c.WeakTypeTag]:Tree	= {
		val superType:Type	= weakTypeOf[S]
		val subType:Type	= weakTypeOf[T]
		q"""
			_root_.scutil.lang.Prism[$superType,$subType](
				get	= it =>
					it match {
						case x:$subType	=> Some(x)
						case _			=> None
					},
				set	= it => it
			)
		"""
	}
}
