package scutil.ext

object SetImplicits extends SetImplicits

trait SetImplicits {
	implicit def toSetExt[S](delegate:Set[S])	= new SetExt(delegate)
}

final class SetExt[S](delegate:Set[S]) {
	def containsAll(that:Set[S]):Boolean	= (delegate & that) == that 
	def containsAny(that:Set[S]):Boolean	= (delegate & that).nonEmpty
	def containsNone(that:Set[S]):Boolean	= (delegate & that).isEmpty 
	
	def mapBy[T](func:S=>T):Map[S,T]	= delegate map { it => (it, func(it)) } toMap
}
