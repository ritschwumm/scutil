package scutil.lang.tc

// import scala.util.TupledFunction

import scutil.lang.tc.Applicative

/*
NOTE links
	@see https://contributors.scala-lang.org/t/mapn-methods-for-common-apply-types/6783/7
	@see https://eed3si9n.com/sudori-part3/
	@see https://contributors.scala-lang.org/t/mapn-methods-for-common-apply-types/6783/7

IDEA
-	we want traverse which is Tuple.map |> _.zipN
-	maybe i can make a summon helper using givens from which i can determine the constructor living in a type member?

PLAN
- implement traverse instead of sequence? then we _have_ the type of F, namely from the operation we traverse with
	- maybe then sequence is easier? for flatten it is when using flatMap
- check generic-tuple-examples.scala
	- does this work because the F is already fixed in the Apply-trait?


NOTE TupledFunction
	F is a function type of arity N
	G is a function with a single tuple argument of size N and its types are equal to the arguments of F
	The return type of F is equal to the return type of G
	F and G are the same sort of function (both are (...) => R or both are (...) ?=> R)
	If only one of F or G is instantiated the second one is inferred.

NOTE existing types

	trait Semigroupal[F[_]] {
		def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
	}

	trait TypeFunc[S] {
		type T
	}

	type InverseMap[X <: Tuple, F[_]] <: Tuple = X match {
		case F[x] *: t => x *: InverseMap[t, F]
		case EmptyTuple => EmptyTuple
	}

	type IsMappedBy[F[_]] = [X <: Tuple] =>> X =:= Map[InverseMap[X, F], F]

NOTE this does not work

	// @see https://contributors.scala-lang.org/t/mapn-methods-for-common-apply-types/6783/7
	extension [Tup <: Tuple](tup: Tup)(using Tuple.Union[Tup] <:< Option[?])
		def mapN[A](f: Tuple.InverseMap[Tup, Option] => A): Option[A] =
			val xs = tup.toList.asInstanceOf[List[Option[Any]]]
			Option.when(xs.forall(_.isDefined)):
				val vs = xs.map(_.get)
				val vtup = vs.foldRight[Tuple](EmptyTuple)(_ *: _).asInstanceOf[Tuple.InverseMap[Tup, Option]]
				f(vtup)
*/

object ApplicativeTuple {
	/*
	Tuple1[F[T1]]				=> F[Tuple1[T1]]
	Tuple2[F[T1], F[T2]]		=> F[Tuple2[T1, T2]]
	Tuple3[F[T1], F[T2], F[T3]]	=> F[Tuple3[T1, T2, T3]]
	*/
	def productN[Tup <: NonEmptyTuple : ApplicativeTuple as at](in:Tup):at.Container[at.Elements]	=
		at.build(in)

	type Aux[In <: NonEmptyTuple, F[_], T]	=
		ApplicativeTuple[In] {
			type Container	= F
			type Elements	= T
		}

	// Tuple1[F[H]]	=> F[Tuple1[H]]
	given [F[_]:Applicative as F, H] => Aux[F[H] *: EmptyTuple, F, H *: EmptyTuple]	=
		new ApplicativeTuple[F[H] *: EmptyTuple] {
			type Container	= F
			type Elements	= H *: EmptyTuple

			def build(in:F[H] *: EmptyTuple):Container[Elements]	=
				F.map(in._1)(Tuple1.apply)
		}

	// TupleN[F[H], ...T]	=> F[TupleN[H, ...]]
	given [F[_]:Applicative as F, H, T <: NonEmptyTuple] => (tailAt:ApplicativeTuple[T] { type Container = F }) => Aux[F[H] *: T, F, H *: tailAt.Elements]	=
		new ApplicativeTuple[F[H] *: T] {
			type Container	= F
			type Elements	= H *: tailAt.Elements

			def build(in:F[H] *: T):Container[Elements]	= {
				val h	= in.head
				val t	= tailAt.build( in.tail)
				F.map2(h, t)(_ *: _)
			}
		}
}

trait ApplicativeTuple[In <: NonEmptyTuple] {
	// the "container" we are building
	type Container[_]

	// the "elements" in the "container"
	type Elements <: Tuple

	def build(in:In):Container[Elements]
}

//-----------------------------------------------------------------------------

/*
def test() = {
	// summon[ApplicativeTuple[Vector[Int] *: EmptyTuple]]
	// summon[ApplicativeTuple[Vector[Int] *: Vector[Int] *: EmptyTuple]]

	// NOTE this does not compile
	// val out	= ApplicativeTuple.productN(Tuple1(None))
	// val out	= ApplicativeTuple.productN(Tuple1(Some(1)))
	// val out	= ApplicativeTuple.productN(Tuple2(Some(1), Some(2)))
	val input	= (Vector(1), Vector(2))
	val out	= ApplicativeTuple.productN(input)
	println(out)
}
*/
