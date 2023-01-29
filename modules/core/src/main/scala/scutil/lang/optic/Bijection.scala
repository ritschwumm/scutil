package scutil.lang

import scala.deriving.Mirror

import scutil.lang.tc.*

object Bijection {
	def identity[T]:Bijection[T,T]	=
		Bijection(Predef.identity, Predef.identity)

	def tuple1[T]:Bijection[Tuple1[T], T]	=
		Bijection(
			it	=> it._1,
			it	=> Tuple1(it)
		)

	// special case for zero elements: convert to Unit instead of EmptyTuple
	def zero[S<:Product](using m:Mirror.ProductOf[S] { type MirroredElemTypes = EmptyTuple }):Bijection[S,Unit]	=
		Bijection(
			s => Tuple.fromProductTyped(s)(using m),
			_ => m.fromProduct(EmptyTuple)
		)

	// special case for single elements: convert to the value itself instead of a a Tuple1
	def one[S<:Product](using m:Mirror.ProductOf[S] { type MirroredElemTypes <: Tuple1[?] }):Bijection[S,Tuple.Head[m.MirroredElemTypes]]	=
		Bijection(
			s => Tuple.fromProductTyped(s)(using m)(0),
			t => m.fromProduct(t *: EmptyTuple)
		)

	def more[S<:Product](using m:Mirror.ProductOf[S]):Bijection[S,m.MirroredElemTypes]	=
		Bijection(
			s => Tuple.fromProductTyped(s)(using m),
			t => m.fromProduct(t)
		)
}

final case class Bijection[S,T](get:S=>T, set:T=>S) {
	// can be used as scala function and extractor
	def apply(t:T):S	= set(t)
	def unapply(s:S):T	= get(s)

	//------------------------------------------------------------------------------

	def mod(func:T=>T):S=>S	= s => set(func(get(s)))
	def modThe(s:S, func:T=>T):S	= mod(func)(s)

	def modF[F[_]](func:T=>F[T])(using F:Functor[F]):S=>F[S]	= s => (F map func(get(s)))(set)
	def modTheF[F[_]:Functor](s:S, func:T=>F[T]):F[S]				= modF(func) apply s

	//------------------------------------------------------------------------------

	def embedState[U](state:State[T,U]):State[S,U]	=
		State { s =>
			val (t,u)	= state run get(s)
			set(t) -> u
		}

	def embedStateT[F[_],U](state:StateT[F,T,U])(using F:Functor[F]):StateT[F,S,U]	=
		StateT { s =>
			val ftu:F[(T,U)]	= state run get(s)
			(F map ftu) { case (t,u) =>
				set(t) -> u
			}
		}

	//------------------------------------------------------------------------------

	def inverse:Bijection[T,S]	=
		Bijection(set, get)

	/** symbolic alias for andThen */
	def >=>[U](that:Bijection[T,U]):Bijection[S,U]	=
		this andThen that

	/** symbolic alias for compose */
	def <=<[R](that:Bijection[R,S]):Bijection[R,T]	=
		this compose that

	def compose[R](that:Bijection[R,S]):Bijection[R,T]	=
		that andThen this

	def andThen[U](that:Bijection[T,U]):Bijection[S,U]	=
		Bijection(
			get	= s	=> that get (this get s),
			set	= u	=> this set  (that set  u)
		)

	def toPrism:Prism[S,T]	=
		Prism.total(get, set)

	def toOptional:Optional[S,T]	=
		Optional.total(get, set)

	def toLens:Lens[S,T]	=
		Lens(
			get	= get,
			set	= t => s => set(t)
		)
}
