package scutil.collection.pimp

import scutil.lang._

object SetImplicits extends SetImplicits

trait SetImplicits {
	implicit final class SetExt[T](peer:Set[T]) {
		def containsAll(that:Set[T]):Boolean	= (peer & that) == that
		def containsAny(that:Set[T]):Boolean	= (peer & that).nonEmpty
		def containsNone(that:Set[T]):Boolean	= (peer & that).isEmpty
		
		/** pairs items only in this with items only in that */
		def hereAndThere(that:Set[T]):(Set[T],Set[T])	=
				(peer -- that, that -- peer)
			
		def where[U](that:Set[T]):Set[Where[T,T]]	= {
			val (here, there)	= this hereAndThere that
			val both			= peer & that
			(both	map { it => Where both	(it,it) })	++
			(here	map { it => Where here	it		})	++
			(there	map { it => Where there	it		})
		}
				
		/** get one element and all other elements */
		def extractSingleOption:Option[(T,Set[T])]	=
				if (peer.nonEmpty)	{
					val head	= peer.head
					Some((head, peer - head))
				}
				else {
					None
				}
			
		/** set or remove the value */
		def set(value:T, in:Boolean):Set[T]	=
				if (in)	peer + value
				else	peer - value
				
		/** create a map from all elements with a given function to generate the values */
		def mapTo[U](value:T=>U):Map[T,U]	=
				(peer map { it => (it, value(it)) }).toMap
			
		/** group values by keys, both from a function */
		def groupMap[K,V](func:T=>(K,V)):Map[K,Set[V]]	=
				peer
				.map		(func)
				.groupBy	{ _._1 }
				.map { case (k, kvs) =>
					(k, kvs map { _._2 })
				}
	}
}