package scutil.ext

object SetImplicits extends SetImplicits

trait SetImplicits {
	implicit def toSetExt[T](delegate:Set[T])	= new SetExt(delegate)
}

final class SetExt[T](delegate:Set[T]) {
	def containsAll(that:Set[T]):Boolean	= (delegate & that) == that 
	def containsAny(that:Set[T]):Boolean	= (delegate & that).nonEmpty
	def containsNone(that:Set[T]):Boolean	= (delegate & that).isEmpty 
	
	/** pairs items only in this with items only in that */
	def hereAndThere(that:Set[T]):(Set[T],Set[T])	=
			(delegate -- that, that -- delegate)
			
	/** set or remove the value */
	def set(value:T, in:Boolean):Set[T]	=
			if (in)	delegate + value
			else	delegate - value
			
	/** create a map from all elements with a given function to generate the values */
	def mapTo[U](value:T=>U):Map[T,U]	=
			delegate map { it => (it, value(it)) } toMap;

}
