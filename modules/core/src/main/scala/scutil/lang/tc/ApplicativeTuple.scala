package scutil.lang.tc

import scutil.lang.tc.Applicative

object ApplicativeTuple {
	/*
	Tuple1[Container[T1]]				=> Container[Tuple1[T1]]
	Tuple2[Container[T1], F[T2]]		=> Container[Tuple2[T1, T2]]
	Tuple3[Container[T1], F[T2], F[T3]]	=> Container[Tuple3[T1, T2, T3]]
	*/
	def productN[Tup <: NonEmptyTuple : ApplicativeTuple as AT](in:Tup):AT.Result	=
		AT.productN(in)

	/*
	Tuple1[Container[S1]]								=> ((S1)			=> U) => Container[U]
	Tuple2[Container[S1], Container[S2]]				=> ((S1, S2)		=> U) => Container[U]
	Tuple3[Container[S1], Container[S2], Container[S3]]	=> ((S1, S2, S3)	=> U) => Container[U]
	*/
	def mapN[In <: NonEmptyTuple : ApplicativeTuple as AT, U](inputs:In)(using NF:NaryFunction[AT.Elements, U])(func:NF.Nary):AT.Container[U]	=
		AT.Applicative.map(
			AT.productN(inputs)
		)(
			NF.tupled(func)
		)

	/*
	Tuple1[T1]			=> ([A] => A => Container[A])	=> Container[Tuple1[T1]]
	Tuple2[T1, T2]		=> ([A] => A => Container[A])	=> Container[Tuple2[T1, T2]]
	Tuple3[T1, T2, T3]	=> ([A] => A => Container[A])	=> Container[Tuple3[T1, T2, T3]]
	*/
	def traverseN[TupT <: Tuple](inputs:TupT)[F[_]](func:[A] => A => F[A])(using AT:ApplicativeTuple[Tuple.Map[inputs.type, F]]):AT.Result	=
		AT.productN(inputs.map(func))

	//-----------------------------------------------------------------------------

	type AuxCE[In <: NonEmptyTuple, F[_], T]	=
		ApplicativeTuple[In] {
			type Container	= F
			type Elements	= T
		}

	type AuxC[In <: NonEmptyTuple, F[_]]	=
		ApplicativeTuple[In] {
			type Container	= F
		}

	// Tuple1[F[H]]	=> F[Tuple1[H]]
	given [F[_]:Applicative as F, H] => AuxCE[F[H] *: EmptyTuple, F, H *: EmptyTuple]	=
		new ApplicativeTuple[F[H] *: EmptyTuple] {
			type Container	= F
			type Elements	= H *: EmptyTuple

			val Applicative	= F

			def productN(in:F[H] *: EmptyTuple):Container[Elements]	=
				F.map(in._1)(Tuple1.apply)
		}

	// TupleN[F[H], ...T]	=> F[TupleN[H, ...]]
	given [F[_]:Applicative as F, H, T <: NonEmptyTuple] => (tailAt:AuxC[T, F]) => AuxCE[F[H] *: T, F, H *: tailAt.Elements]	=
		new ApplicativeTuple[F[H] *: T] {
			type Container	= F
			type Elements	= H *: tailAt.Elements

			val Applicative	= F

			def productN(in:F[H] *: T):Container[Elements]	= {
				val h	= in.head
				val t	= tailAt.productN(in.tail)
				F.map2(h, t)(_ *: _)
			}
		}
}

trait ApplicativeTuple[In] {
	// the "container" we are building
	type Container[_]

	// the "elements" in the "container"
	type Elements <: Tuple

	// the "container" with its "elements"
	type Result	= Container[Elements]

	//-----------------------------------------------------------------------------

	val Applicative:Applicative[Container]

	/*
	Tuple1[Container[T1]]								=> Container[Tuple1[T1]]
	Tuple2[Container[T1], Container[T2]]				=> Container[Tuple2[T1, T2]]
	Tuple3[Container[T1], Container[T2], Container[T3]]	=> Container[Tuple3[T1, T2, T3]]
	*/
	def productN(in:In):Result
}
