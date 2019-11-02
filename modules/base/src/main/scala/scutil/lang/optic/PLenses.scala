package scutil.lang

import scutil.lang.implicits._
import scutil.collection.implicits._

object PLenses {
	def opt[S,T](total:Lens[S,Option[T]]):PLens[S,T]	=
			PLens { s =>
				total get s map { t =>
					Store[S,T](
						t,
						t => total set Some(t) apply s
					)
				}
			}

	//------------------------------------------------------------------------------

	@deprecated("use seq", "0.162.0")
	def iseq[T](i:Int):PLens[ISeq[T],T]	=
			seq(i)

	def seq[T](i:Int):PLens[Seq[T],T]	=
			PLens { _ storeAt i }

	def map[K,V](k:K):PLens[Map[K,V],V]	=
			PLens { _ storeAt k }

	def nes[T](i:Int):PLens[Nes[T],T]	=
			PLens { _ storeAt i }

	//------------------------------------------------------------------------------

	@deprecated("use seqWhere", "0.162.0")
	def iseqWhere[T](pred:Predicate[T]):PLens[ISeq[T],T]	=
			seqWhere(pred)

	@deprecated("use seqWhereEqual", "0.162.0")
	def iseqWhereEqual[S,T](extract:S=>T, id:T):PLens[ISeq[S],S]	=
			seqWhereEqual(extract, id)

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
