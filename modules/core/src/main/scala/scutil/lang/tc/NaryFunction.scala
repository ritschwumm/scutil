package scutil.lang.tc

import scala.util.TupledFunction

object NaryFunction {
	// calculate a tupled function from a non-tupled one without
	// having to know the type of that function up front as with TupledFunction
	def tupled[TupS <: Tuple, Out](using nf:NaryFunction[TupS, Out])(nary:nf.Nary):TupS	=> Out	=
		nf.tupled(nary)

	type Aux[TupS <: Tuple, Out, Nary0]	=
		NaryFunction[TupS, Out] {
			type Nary	= Nary0
		}

	given [TupS <: Tuple, Out, Nary0] => (tf:TupledFunction[Nary0, TupS=>Out]) => Aux[TupS, Out, Nary0]	=
		new NaryFunction[TupS, Out] {
			type Nary	= Nary0

			def tupled(nary:Nary):TupS => Out	= tf.tupled(nary)
		}
}

// type function to calculate a non-tupled function from its tupled equivalent
trait NaryFunction[TupS <: Tuple, Out] {
	// (...TupS) => T
	type Nary

	def tupled(nary:Nary):TupS => Out
}
