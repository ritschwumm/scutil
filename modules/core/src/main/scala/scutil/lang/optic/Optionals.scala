package scutil.lang

import scutil.lang.implicits.*
import scutil.collection.implicits.*

object Optionals {
	def seq[T](i:Int):Optional[Seq[T],T]	=
		Optional(
			get	= s			=> s lift i,
			set	= t => s	=> s.updatedAt(i, t) getOrElse s
		)

	def map[K,V](k:K):Optional[Map[K,V],V]	=
		Optional(
			get	= s			=> s get k,
			set	= t => s	=> s.updatedAt(k, t) getOrElse s
		)

	def nes[T](i:Int):Optional[Nes[T],T]	=
		Optional(
			get	= s			=> s get i,
			set	= t => s	=> s.updatedAt (i, t) getOrElse s
		)

	//------------------------------------------------------------------------------

	def seqWhere[T](pred:Predicate[T]):Optional[Seq[T],T]	=
		Optional(
			get	= items			=> items find pred,
			set	= item => items	=> items.indexWhereOption(pred).cata(items, index => items.updated(index, item))
		)

	def seqWhereEqual[S,T](extract:S=>T, id:T):Optional[Seq[S],S]	=
		seqWhere { it =>
			extract(it) ==== id
		}

	def seqHead[T]:Optional[Seq[T],T]	=
		Optional(
			get	=			s	=> s.headOption,
			set	=	t	=>	s	=> if (s.nonEmpty) s.updated(0, t) else s
		)

	def seqLast[T]:Optional[Seq[T],T]	=
		Optional(
			get	=			s	=> s.lastOption,
			set	=	t	=>	s	=> if (s.nonEmpty) s.updated(s.size-1, t) else s
		)
}
