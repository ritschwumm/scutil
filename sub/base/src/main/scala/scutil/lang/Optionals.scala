package scutil.lang

import scutil.lang.implicits._
import scutil.collection.implicits._

object Optionals {
	// TODO optics add ISeq#head and ISeq#last
	
	def opt[S,T](total:Lens[S,Option[T]]):Optional[S,T]	=
			Optional(
				get	= total.get,
				put	= (s,t) => total put (s, Some(t))
			)
	
	//------------------------------------------------------------------------------
	
	def iseq[T](i:Int):Optional[ISeq[T],T]	=
			Optional(
				get	= s 		=> s lift i,
				put	= (s, t)	=> s updatedAt (i, t) getOrElse s
			)
			
	def map[K,V](k:K):Optional[Map[K,V],V]	=
			Optional(
				get	= s 		=> s get k,
				put	= (s, t)	=> s updatedAt (k, t) getOrElse s
			)
		
	def nes[T](i:Int):Optional[Nes[T],T]	=
			Optional(
				get	= s 		=> s get i,
				put	= (s, t)	=> s updatedAt (i, t) getOrElse s
			)
			
	//------------------------------------------------------------------------------
	
	def iseqWhere[T](pred:Predicate[T]):Optional[ISeq[T],T]	=
			Optional(
				get	= items			=> items find pred,
				put	= (items, item)	=> items indexWhereOption pred  cata (items, index => items updated (index, item))
			)
				
	def iseqWhereEqual[S,T](extract:S=>T, id:T):Optional[ISeq[S],S]	=
			iseqWhere { it =>
				extract(it) ==== id
			}
}
