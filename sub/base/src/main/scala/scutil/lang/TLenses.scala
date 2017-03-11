package scutil.lang

import scutil.collection.implicits._

object TLenses {
	def pairFirst[S,T]:TLens[(S,T),S]	=
			TLens create (
				p		=> p._1,
				(p, t)	=> (t, p._2)
			)

	def pairSecond[S,T]:TLens[(S,T),T]	=
			TLens create (
				p		=> p._2,
				(p, t)	=> (p._1, t)
			)
	
	def set[T](t:T):TLens[Set[T],Boolean]	=
			TLens create (
				_ contains t,
				(c, v)	=> if (v) c + t else c - t
			)
						
	def map[K,V](k:K):TLens[Map[K,V],Option[V]]	=
			TLens { _ optionStoreAt k }
		
	def mapWithDefault[S,T](key:S, default:T):TLens[Map[S,T],T]	=
			TLens { map =>
				Store(
					get	= map get key getOrElse default,
					put	= value =>
						if (value != default)	map updated (key, value)
						else					map - key
				)
			}
}
