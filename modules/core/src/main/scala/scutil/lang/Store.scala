package scutil.lang

import scutil.lang.tc._

object Store {
	def identity[T](value:T):Store[T,T]	=
		Store(value, t => t)

	def trivial[T](value:T):Store[Unit,T]	=
		Store((), _ => value)

	//------------------------------------------------------------------------------
	//## typeclass instances

	given StoreFunctor[S]:Functor[Store[S,_]]	=
		new Functor[Store[S,_]] {
			def map[A,B](it:Store[S,A])(func:A=>B):Store[S,B]	= it map func
		}
}

final case class Store[V,C](index:V, peek:V=>C) {
	def extract:C	= peek(index)

	def modify(func:V=>V):C		= peek(func(index))

	def modifyF[F[_]](func:V=>F[V])(using F:Functor[F]):F[C]	=
		(F map func(index))(peek)

	def modifyState[X](func:State[V,X]):(C,X)	= {
		val (v2, side)	= func run index
		(peek(v2), side)
	}

	def modifyStateT[F[_],X](func:StateT[F,V,X])(using F:Functor[F]):F[(C,X)]	=
		(F map (func run index)) { case (v, x) => (peek(v), x) }

	//------------------------------------------------------------------------------

	// aka coFlatten
	def duplicate:Store[V,Store[V,C]]	=
		Store(index, Store(_, peek))

	def map[CC](func:C=>CC):Store[V,CC]	=
		Store[V,CC](
			index,
			peek andThen func
		)

	// TODO does this have a better name?
	def coFlatMap[CC](func:Store[V,C]=>CC):Store[V,CC] =
		Store[V,CC](
			index,
			x => func(Store(x, peek))
		)

	/** symbolic alias for andThen */
	def >=>[VV](that:Store[VV,V]):Store[VV,C]	=
		this andThen that

	/** symbolic alias for compose */
	def <=<[CC](that:Store[C,CC]):Store[V,CC]	=
		this compose that

	def andThen[VV](that:Store[VV,V]):Store[VV,C]	=
		Store(
			that.index,
			that.peek andThen this.peek
		)

	def compose[CC](that:Store[C,CC]):Store[V,CC]	=
		that andThen this

	// TODO optics cleanup
	def andThenBijection[U](that:Bijection[V,U]):Store[U,C]	=
		Store(
			that get index,
			that.set andThen peek
		)
}
