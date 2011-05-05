package scutil.ext

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom

trait TraversableLikeImplicits {
	implicit def toTraversableLikeExt[T,C[T]<:TraversableLike[T,C[T]]](delegate:C[T]) = new TraversableLikeExt[T,C](delegate)
}

final class TraversableLikeExt[T,C[T]<:TraversableLike[T,C[T]]](delegate:C[T]) {
	/** pair elements of a collection with a function applied to an element */
	def zipBy[T1>:T,U,That](func:T=>U)(implicit bf:CanBuildFrom[C[T],(T1,U),That]):That	= {
		delegate map { it => (it, func(it)) }
	}
	
	// NOTE this should be generalized to other AFs, not just Option
	
	/** Delegate is traversable (in the haskell sense), Option is an idiom. */
	def sequenceOption[EL,That](implicit ev:T=>Option[EL], bf:CanBuildFrom[C[T],EL,That]):Option[That]	= {
		if (delegate forall { _.isDefined })	Some(delegate map { _.get })
		else									None
	}
}
