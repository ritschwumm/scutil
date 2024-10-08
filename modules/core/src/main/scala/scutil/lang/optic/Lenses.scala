package scutil.lang

import scutil.collection.implicits.*

object Lenses {
	def set[T](t:T):Lens[Set[T],Boolean]	=
		Lens(
			get	= _ contains t,
			set	= v => c	=> if (v) c + t else c - t
		)

	def map[K,V](k:K):Lens[Map[K,V],Option[V]]	=
		Lens.fromStoreAt(_.optionStoreAt(k))

	/*
	// TODO check this works
	def mapWithDefault2[S,T](key:S, default:T):Lens[Map[S,T],T]	=
		map(key) >=> (Bijections withDefault default).toLens
	*/

	def mapWithDefault[S,T](key:S, default:T):Lens[Map[S,T],T]	=
		Lens(
			get	= map =>
				map.get(key).getOrElse(default),
			set	= value => map =>
				if (value != default)	map.updated(key, value)
				else					map - key
		)
}
