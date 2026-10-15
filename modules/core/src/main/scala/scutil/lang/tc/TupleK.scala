package scutil.lang.tc

import scutil.lang.*
import scutil.lang.tc.Applicative

object TupleK {
	/*
	Tuple1[F[T1]]				=> F[Tuple1[T1]]
	Tuple2[F[T1], F[T2]]		=> F[Tuple2[T1, T2]]
	Tuple3[F[T1], F[T2], F[T3]]	=> F[Tuple3[T1, T2, T3]]
	*/
	def productN[TupT <: Tuple](inputs:TupT)(using TK:TupleK[TupT], AP:Applicative[TK.Container]):TK.Result	=
		TK.productN(inputs)

	/*
	Tuple1[F[S1]]				=> ((S1)			=> U) => F[U]
	Tuple2[F[S1], F[S2]]		=> ((S1, S2)		=> U) => F[U]
	Tuple3[F[S1], F[S2], F[S3]]	=> ((S1, S2, S3)	=> U) => F[U]
	*/
	def mapN[In <: Tuple, U](inputs:In)(using TK:TupleK[In], AP:Applicative[TK.Container], NF:NaryFunction[TK.Elements, U])(func:NF.Nary):TK.Container[U]	=
		AP.map(
			TK.productN(inputs)
		)(
			NF.tupled(func)
		)

	/*
	Tuple1[T1]			=> ([A] => A => F[A])	=> F[Tuple1[T1]]
	Tuple2[T1, T2]		=> ([A] => A => F[A])	=> F[Tuple2[T1, T2]]
	Tuple3[T1, T2, T3]	=> ([A] => A => F[A])	=> F[Tuple3[T1, T2, T3]]
	*/
	def traverseN[TupT <: Tuple](inputs:TupT)[F[_]](func:[A] => A => F[A])(using TK:TupleK[Tuple.Map[inputs.type, F]], AP:Applicative[TK.Container]):TK.Result	=
		TK.productN(inputs.map(func))

	/*
	in contrast to map, this takes a natural transformation instead of a constructor

	Tuple1[F[T1]]				=> (F ~> G)	=> Tuple1[G[T1]]
	Tuple2[F[T1], F[T2]]		=> (F ~> G)	=> Tuple2[G[T1], G[T2]]
	Tuple3[F[T1], F[T2], F[T3]]	=> (F ~> G)	=> Tuple3[G[T1], G[T2], G[T3]]
	*/
	def mapK[TupT <: Tuple](inputs:TupT)(using TK:TupleK[TupT])[G[_]](func:TK.Container ~> G):Tuple.Map[TK.Elements, G]	=
		TK.mapK(inputs)(func)

	/*
	in contrast to traverseN, this takes a natural transformation instead of a constructor

	Tuple1[F[T1]]				=> (F ~> G)	=> G[Tuple1[T1]]
	Tuple2[F[T1], F[T2]]		=> (F ~> G)	=> G[Tuple2[T1, T2]]
	Tuple3[F[T1], F[T2], F[T3]]	=> (F ~> G)	=> G[Tuple3[T1, T2, T3]]
	*/
	def traverseK[TupT <: Tuple](inputs:TupT)(using TK:TupleK[TupT])[G[_]](func:TK.Container ~> G)(using UK:TupleK[Tuple.Map[TK.Elements, G]], AP:Applicative[UK.Container]):UK.Result	=
		productN(mapK(inputs)(func))

	//-----------------------------------------------------------------------------

	type AuxCE[In <: Tuple, F[_], T]	=
		TupleK[In] {
			type Container	= F
			type Elements	= T
		}

	type AuxC[In <: Tuple, F[_]]	=
		TupleK[In] {
			type Container	= F
		}

	// Tuple1[F[H]]	=> F[Tuple1[H]]
	given [F[_], H] => AuxCE[F[H] *: EmptyTuple, F, H *: EmptyTuple]	=
		new TupleK[F[H] *: EmptyTuple] {
			type Container	= F
			type Elements	= H *: EmptyTuple

			def mapK(in:F[H] *: EmptyTuple)[G[_]](func:F ~> G):Tuple.Map[Elements, G]	=
				func(in.head) *: EmptyTuple

			def productN(in:F[H] *: EmptyTuple)(using AP:Applicative[Container]):Result	=
				AP.map(in.head)(_ *: EmptyTuple)
		}

	// TupleN[F[H], ...T]	=> F[TupleN[H, ...]]
	given [F[_], H, T <: NonEmptyTuple] => (tailAt:AuxC[T, F]) => AuxCE[F[H] *: T, F, H *: tailAt.Elements]	=
		new TupleK[F[H] *: T] {
			type Container	= F
			type Elements	= H *: tailAt.Elements

			def mapK(in:F[H] *: T)[G[_]](func:F ~> G):Tuple.Map[Elements, G]	=
				func(in.head) *: tailAt.mapK(in.tail)(func)

			def productN(in:F[H] *: T)(using AP:Applicative[Container]):Result	= {
				val h	= in.head
				val t	= tailAt.productN(in.tail)
				AP.map2(h, t)(_ *: _)
			}
		}
}

/*
evidence that each Element of the Tuple In is wrapped in a specific Container.

this can be used in cases where Tuple.InverseMap hinders type inference, e.g
because the F[_] parameter cannot be inferred.

in addition to the un-wrapped types of the Elements, this also returns the
type(-constructor) of the Container
*/
trait TupleK[In <: Tuple] {
	// the "container" we are building
	type Container[_]

	// the "elements" in the "container"
	type Elements <: Tuple

	// the "container" with its "elements"
	type Result	= Container[Elements]

	//-----------------------------------------------------------------------------

	/*
	in contrast to map, this takes a natural transformation instead of a constructor

	Tuple1[F[T1]]				=> (F ~> G)	=> Tuple1[G[T1]]
	Tuple2[F[T1], F[T2]]		=> (F ~> G)	=> Tuple2[G[T1], G[T2]]
	Tuple3[F[T1], F[T2], F[T3]]	=> (F ~> G)	=> Tuple3[G[T1], G[T2], G[T3]]
	*/
	def mapK(in:In)[G[_]](func:Container ~> G):Tuple.Map[Elements, G]

	/*
	Tuple1[F[T1]]				=> F[Tuple1[T1]]
	Tuple2[F[T1], F[T2]]		=> F[Tuple2[T1, T2]]
	Tuple3[F[T1], F[T2], F[T3]]	=> F[Tuple3[T1, T2, T3]]
	*/
	def productN(in:In)(using AP:Applicative[Container]):Result
}
