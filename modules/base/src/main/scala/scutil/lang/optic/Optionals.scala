package scutil.lang

import scutil.lang.implicits._
import scutil.collection.implicits._

object Optionals {
	def opt[S,T](total:Lens[S,Option[T]]):Optional[S,T]	=
			Optional(
				get	= total.get,
				set	= t => total set Some(t)
			)
	
	//------------------------------------------------------------------------------
	
	def iseq[T](i:Int):Optional[ISeq[T],T]	=
			Optional(
				get	= s 		=> s lift i,
				set	= t => s	=> s updatedAt (i, t) getOrElse s
			)
			
	def map[K,V](k:K):Optional[Map[K,V],V]	=
			Optional(
				get	= s 		=> s get k,
				set	= t => s	=> s updatedAt (k, t) getOrElse s
			)
		
	def nes[T](i:Int):Optional[Nes[T],T]	=
			Optional(
				get	= s 		=> s get i,
				set	= t => s	=> s updatedAt (i, t) getOrElse s
			)
			
	//------------------------------------------------------------------------------
	
	def iseqWhere[T](pred:Predicate[T]):Optional[ISeq[T],T]	=
			Optional(
				get	= items			=> items find pred,
				set	= item => items	=> items indexWhereOption pred  cata (items, index => items updated (index, item))
			)
				
	def iseqWhereEqual[S,T](extract:S=>T, id:T):Optional[ISeq[S],S]	=
			iseqWhere { it =>
				extract(it) ==== id
			}
			
	def iseqHead[T]:Optional[ISeq[T],T]	=
			Optional(
				get	=			s	=> s.headOption,
				set	=	t	=>	s	=> if (s.nonEmpty) s updated (0, t) else s
			)
			
	def iseqLast[T]:Optional[ISeq[T],T]	=
			Optional(
				get	=			s	=> s.lastOption,
				set	=	t	=>	s	=> if (s.nonEmpty) s updated (s.size-1, t) else s
			)
}
