package scutil.ext

object PartialFunctionImplicits extends PartialFunctionImplicits

trait PartialFunctionImplicits {
	implicit def toPartialFunctionExt[S,T](delegate:PartialFunction[S,T])	= new PartialFunctionExt[S,T](delegate)
}

final class PartialFunctionExt[S,T](delegate:PartialFunction[S,T]) {
	def orAlways(func:S=>T):S=>T	= it =>
			if (delegate isDefinedAt it)	delegate(it) 
			else							func(it)
}
