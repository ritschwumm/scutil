package scutil.lang.extension

import scutil.lang.tc.Applicative
import scutil.lang.tc.TupleK
import scutil.lang.tc.NaryFunction

object TupleExtensions {
	//-----------------------------------------------------------------------------
	//## pairwise application

	/* takes a Tuple of inputs and a Tuple of functions from input to output and returns a Tuple of outputs */
	type Applied[TupS <: Tuple, TupF <: Tuple] <: Tuple	=
		(TupS, TupF) match {
			case (s *: ss, f *: ff)	=>
				f match {
					case (s => t)	=> t *: Applied[ss, ff]
				}
			case Any => EmptyTuple
		}

	/* pairwise apply functions from the funcs tuple to values from the inputs tuple and return the results in an output tuple */
	extension [TupS <: Tuple](inputs:TupS) {
		@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
		def ap[TupF <: Tuple](funcs:TupF):Applied[TupS, TupF]	= {
			val size	= inputs.productArity `min` funcs.productArity
			val outputs	= new Array[Any](size)
			var i = 0
			while i < size do {
				outputs(i)	= funcs(i).asInstanceOf[Any=>Any](inputs(i))
				i	+= 1
			}
			Tuple.fromArray(outputs).asInstanceOf[Applied[TupS, TupF]]
		}
	}

	//-----------------------------------------------------------------------------
	//## applicative traversal

	extension [TupT <: NonEmptyTuple : TupleK as TK](inputs:TupT) {
		def productN(using Applicative[TK.Container]):TK.Result	=
			TK.productN(inputs)

		def mapN[U](using AP:Applicative[TK.Container], NF:NaryFunction[TK.Elements, U])(func:NF.Nary):TK.Container[U]	=
			TupleK.mapN(inputs)(func)
	}

	extension [TupT <: Tuple](inputs:TupT) {
		def traverseN[F[_]](func:[A] => A => F[A])(using TK:TupleK[Tuple.Map[inputs.type, F]], AP:Applicative[TK.Container]):TK.Result	=
			TupleK.traverseN(inputs)(func)
	}
}
