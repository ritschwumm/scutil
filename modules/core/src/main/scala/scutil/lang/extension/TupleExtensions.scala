package scutil.lang.extension

import scutil.lang.tc.Applicative
import scutil.lang.tc.ApplicativeTuple
import scutil.lang.tc.NaryFunction

object TupleExtensions {
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

	extension [TupT <: NonEmptyTuple : ApplicativeTuple as at](inputs:TupT) {
		def zipN:at.Container[at.Elements]	= at.build(inputs)

		def mapN[U](using ap:Applicative[at.Container], nf:NaryFunction[at.Elements, U])(func:nf.Nary):at.Container[U]	=
			ap.map(zipN)(
				nf.tupled(func)
			)
	}


	//-----------------------------------------------------------------------------

	/*
	// NOTE TupledFunction disallows inferring the types of the function parameters
	extension [F[_]:Applicative as F, FSS <: Tuple:Tuple.IsMappedBy[F]](fss:FSS) {
		def mapN2[C, T](func:C)(using tf:TupledFunction[C, Tuple.InverseMap[FSS, F] => T]):F[T] =
			F.map(zipN2(fss))(tf.tupled(func))
	}

	// NOTE fails to determine the Applicative at the call site, ApplicativeTuple works though
	extension [SS <: Tuple](ss:SS) {
		def zipN2[F[_]](using ev:Tuple.IsMappedBy[F][SS], F:Applicative[F]):F[Tuple.InverseMap[SS, F]] = {
			def loop[E <: Tuple](it:E):F[Tuple]	=
				it match{
					case EmptyTuple	=> F.pure(EmptyTuple)
					case h *: t		=> F.map2(h.asInstanceOf[F[Any]], loop(t.asInstanceOf[Tuple]) )(_ *: _)
				}
			loop(ss).asInstanceOf[F[Tuple.InverseMap[SS, F]]]
		}
	}
	*/
}
