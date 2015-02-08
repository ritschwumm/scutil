package scutil.lang

import scutil.lang.implicits._
import scutil.collection.implicits._

object PLenses {
	def opt[S,T](total:TLens[S,Option[T]]):PLens[S,T]	=
			PLens { s =>
				total get s map { t =>
					Store[S,T](
						t,
						t => total put (s, Some(t))
					)
				}
			}
			
	//------------------------------------------------------------------------------
	
	def iseq[T](i:Int):PLens[ISeq[T],T]	=
			PLens { s =>
				s	containsIndex
				i	guard
				Store[ISeq[T],T](
					s(i),
					v => s updated (i, v)
				)
			}
			
	def map[K,V](k:K):PLens[Map[K,V],V]	=
			PLens { s =>
				s	contains
				k	guard
				Store[Map[K,V],V](
					s(k),
					v => s updated (k, v)
				)
			}
			
	//------------------------------------------------------------------------------
	
	def iseqWhere[T](pred:Predicate[T]):PLens[ISeq[T],T]	=
			PLens { items =>
				items indexWhereOption pred flatMap {
					PLenses iseq _ on items
				}
			}
				
	def iseqWhereEqual[S,T](extract:S=>T, id:T):PLens[ISeq[S],S]	=
			iseqWhere { it =>
				extract(it) ==== id
			}
}
