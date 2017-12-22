package scutil.lang

import scutil.collection.implicits._

object Lenses {
	def pairFirst[S,T]:Lens[(S,T),S]	=
			Lens(
				get	= p			=> p._1,
				set	= t => p	=> (t, p._2)
			)

	def pairSecond[S,T]:Lens[(S,T),T]	=
			Lens(
				get	= p			=> p._2,
				set	=  t => p	=> (p._1, t)
			)
	
	def set[T](t:T):Lens[Set[T],Boolean]	=
			Lens(
				get	= _ contains t,
				set	= v => c	=> if (v) c + t else c - t
			)
						
	def map[K,V](k:K):Lens[Map[K,V],Option[V]]	=
			Lens fromStoreAt (_ optionStoreAt k)
		
	def mapWithDefault[S,T](key:S, default:T):Lens[Map[S,T],T]	=
			Lens(
				get	= map =>
						map get key getOrElse default,
				set	= value => map =>
						if (value != default)	map updated (key, value)
						else					map - key
			)
}
