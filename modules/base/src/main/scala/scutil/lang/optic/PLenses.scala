package scutil.lang

import scutil.lang.implicits._
import scutil.collection.implicits._

object PLenses {
	@deprecated("use PLens.some", "0.185.0")
	def opt[S,T](total:Lens[S,Option[T]]):PLens[S,T]	= PLens.some(total)

	//------------------------------------------------------------------------------

	def seq[T](i:Int):PLens[Seq[T],T]	=
		PLens { _ storeAt i }

	def map[K,V](k:K):PLens[Map[K,V],V]	=
		PLens { _ storeAt k }

	def nes[T](i:Int):PLens[Nes[T],T]	=
		PLens { _ storeAt i }

	//------------------------------------------------------------------------------

	def seqWhere[T](pred:Predicate[T]):PLens[Seq[T],T]	=
		PLens { items =>
			items indexWhereOption pred flatMap {
				PLenses seq _ on items
			}
		}

	def seqWhereEqual[S,T](extract:S=>T, id:T):PLens[Seq[S],S]	=
		seqWhere { it =>
			extract(it) ==== id
		}
}
