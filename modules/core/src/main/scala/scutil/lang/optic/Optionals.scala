package scutil.lang

import scala.collection.Factory
import scala.collection.generic.IsSeq

import scutil.lang.implicits.*
import scutil.collection.implicits.*

object Optionals {
	//-----------------------------------------------------------------------------
	//## Option

	def some[T]:Optional[Option[T], T]	=
		Optional(
			get	= identity,
			set	= (t:T) => (s:Option[T]) => s.map(_ => t)
		)

	//-----------------------------------------------------------------------------
	//## Either

	def left[L,R]:Optional[Either[L,R], L]	=
		Optional(
			get	= _.leftToOption,
			set	= (t:L) => (s:Either[L,R]) => s.leftMap(_ => t)
		)

	def right[L,R]:Optional[Either[L,R], R]	=
		Optional(
			get	= _.toOption,
			set	= (t:R) => (s:Either[L,R]) => s.map(_ => t)
		)

	//-----------------------------------------------------------------------------
	//## Seq

	def seq[T](i:Int):Optional[Seq[T],T]	=
		Optional(
			get	= s			=> s.lift(i),
			set	= t => s	=> s.updatedAt(i, t).getOrElse(s)
		)

	def seqWhere[T](pred:Predicate[T]):Optional[Seq[T],T]	=
		Optional(
			get	= items			=> items.find(pred),
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

	//-----------------------------------------------------------------------------
	//## Vector

	def vector[T](i:Int):Optional[Vector[T],T]	=
		Optional(
			get	= s			=> s.lift(i),
			set	= t => s	=> s.updatedAt(i, t).getOrElse(s)
		)

	def vectorWhere[T](pred:Predicate[T]):Optional[Vector[T],T]	=
		Optional(
			get	= items			=> items.find(pred),
			set	= item => items	=> items.indexWhereOption(pred).cata(items, index => items.updated(index, item))
		)

	def vectorWhereEqual[S,T](extract:S=>T, id:T):Optional[Vector[S],S]	=
		vectorWhere { it =>
			extract(it) ==== id
		}

	def vectorHead[T]:Optional[Vector[T],T]	=
		Optional(
			get	=			s	=> s.headOption,
			set	=	t	=>	s	=> if (s.nonEmpty) s.updated(0, t) else s
		)

	def vectorLast[T]:Optional[Vector[T],T]	=
		Optional(
			get	=			s	=> s.lastOption,
			set	=	t	=>	s	=> if (s.nonEmpty) s.updated(s.size-1, t) else s
		)

	//-----------------------------------------------------------------------------
	//## IndexedSeq

	def indexed[F[T] <: IndexedSeq[T], T](i:Int)(using factory:Factory[T, F[T]]):Optional[F[T],T]	=
		Optional(
			get	= s			=> s.lift(i),
			set	= t => s	=>
				if (i >= 0 && i < s.size)	factory.fromSpecific(s.updated(i, t))
				else s
		)

	//-----------------------------------------------------------------------------
	//## Nes

	def nes[T](i:Int):Optional[Nes[T],T]	=
		Optional(
			get	= s			=> s.get(i),
			set	= t => s	=> s.updatedAt(i, t).getOrElse(s)
		)

	//-----------------------------------------------------------------------------
	//## Map

	def map[K,V](k:K):Optional[Map[K,V],V]	=
		Optional(
			get	= s			=> s.get(k),
			set	= t => s	=> s.updatedAt(k, t).getOrElse(s)
		)
}
