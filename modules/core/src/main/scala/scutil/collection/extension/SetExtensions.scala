package scutil.collection.extension

import scutil.lang.*

object SetExtensions {
	implicit final class SetExt[T](peer:Set[T]) {
		def containsAll(that:Set[T]):Boolean	= (peer & that) == that
		def containsAny(that:Set[T]):Boolean	= (peer & that).nonEmpty
		def containsNone(that:Set[T]):Boolean	= (peer & that).isEmpty

		/** pairs items only in this with items only in that */
		def hereAndThere(that:Set[T]):(Set[T],Set[T])	=
			(peer -- that, that -- peer)

		def ior[U](that:Set[T]):Set[Ior[T,T]]	= {
			val (here, there)	= this hereAndThere that
			val both			= peer & that

			both	.map { it => Ior.both	(it,it) }	++
			here	.map { it => Ior.left	(it)	}	++
			there	.map { it => Ior.right	(it)	}
		}

		/** get one element and all other elements */
		def extractSingleOption:Option[(T,Set[T])]	=
			peer.headOption map { head => (head, peer - head) }

		/** set or remove the value */
		def set(value:T, in:Boolean):Set[T]	=
			if (in)	peer + value
			else	peer - value

		/** create a map from all elements with a given function to generate the values */
		def mapTo[U](value:T=>U):Map[T,U]	=
			(peer map { it => (it, value(it)) }).toMap

		/** group values by keys, both from a function */
		def groupMapPaired[K,V](func:T=>(K,V)):Map[K,Set[V]]	=
			peer
			.map		(func)
			.groupBy	{ _._1 }
			.map { case (k, kvs) =>
				(k, kvs map { _._2 })
			}
	}
}
