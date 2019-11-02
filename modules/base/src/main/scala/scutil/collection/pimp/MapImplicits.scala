package scutil.collection.pimp

import scutil.lang._

object MapImplicits extends MapImplicits

trait MapImplicits {
	implicit final class MapExt[S,T](peer:Map[S,T]) {
		// aliases with low precendence

		def put(key:S, value:T):Map[S,T]	= peer + (key -> value)
		def remove(key:S):Map[S,T]			= peer - key

		/** remove an element and return it */
		def extractAt(s:S):Option[(T,Map[S,T])]	=
				peer get s map { t => (t, peer - s) }

		def partitionKeys(pred:Predicate[S]):(Map[S,T],Map[S,T])	=
				peer partition { case (k, _)	=> pred(k) }

		/** filterKeys keeps a reference to the original Map, this does not */
		def strictFilterKeys(pred:Predicate[S]):Map[S,T]	=
				peer filter { case (s, _)	=> pred(s) }

		/** inverse strictFilterKeys */
		def strictFilterNotKeys(pred:Predicate[S]):Map[S,T]	=
				peer filter { case (s, _)	=> !pred(s) }

		/** mapValues keeps a reference to the original Map, this does not */
		def strictMapValues[U](func:T=>U):Map[S,U]	=
				peer map { case (s, t) => (s, func(t)) }

		/*
		// elegant, but dog slow
		def where[U](that:Map[S,U]):Map[S,Where[T,U]]	=
				(peer.keySet ++ that.keySet)
				.map { k =>
					k -> (Where from (peer get k, that get k) getOrElse nothing)
				}
				.toMap
		*/
		def where[U](that:Map[S,U]):Map[S,Where[T,U]]	= {
			var out	= Map.empty[S,Where[T,U]]
			peer foreach { case (s, t) =>
				that get s match {
					case Some(u)	=> out += (s -> Both(t, u))
					case None		=> out += (s -> Here(t))
				}
			}
			that foreach { case (s, u) =>
				peer get s match {
					case Some(t1)	=>
					case None		=> out += (s -> There(u))
				}
			}
			out
		}

		/** set or remove the value */
		def set(key:S, value:Option[T]):Map[S,T]	=
				value match {
					case Some(value)	=> peer + (key -> value)
					case None			=> peer - key
				}

		/** set or remove multiple values */
		def setMany(it:Iterable[(S,Option[T])]):Map[S,T]	=
				(it foldLeft peer) { (orig, change) =>
					change match {
						case (k, Some(v))	=> orig + (k -> v)
						case (k, None)		=> orig - k
					}
				}

		def setManyBy(func:(S,T)=>Option[T]):Map[S,T]	=
				peer flatMap { case (k,v) =>
					(func(k, v) map { k -> _ }).toList
				}

		/** set or remove the value for a single key */
		def setBy(key:S, func:PEndo[T]):Map[S,T]	=
				peer get key flatMap func match {
					case Some(value)	=> peer + (key -> value)
					case None			=> peer
				}

		/** map the value for a single key */
		def updatedBy(key:S, func:Endo[T]):Option[Map[S,T]]	=
				peer get key match {
					case Some(value)	=> Some(peer + (key -> func(value)))
					case None			=> None
				}

		def updatedManyBy(func:(S,T)=>T):Map[S,T]	=
				peer map { case (k,v) =>
					k -> func(k, v)
				}

		/** None when the key does not exist */
		def updatedAt(key:S, value:T):Option[Map[S,T]]	=
				if (peer contains key)	Some(peer updated (key, value))
				else					None

		/** map the value for a single key if it exists or insert a new value for this key */
		def updatedByOrInserted(key:S, update:Endo[T], insert: =>T):Map[S,T]	=
				peer + (key -> (peer get key map update getOrElse insert))

		def putIfNotExist(key:S, value:T):(Map[S,T],Boolean)	=
				if (peer contains key)	(peer, false)
				else					(peer + (key -> value), true)

		def storeAt(key:S):Option[Store[Map[S,T],T]]	=
				peer get key map { item	=>
					Store[Map[S,T],T](
						item,
						peer updated (key, _)
					)
				}

		def optionStoreAt(key:S):Store[Map[S,T],Option[T]]	=
				Store(
					peer get key,
					set(key, _)
				)
	}
}