package scutil.math.extension

import scala.annotation.tailrec

object OrderingExtensions {
	extension (peer:Ordering.type) {
		/**
		treats everything as equal,
		forms a Monoid with OrderingExt#orElse
		*/
		def trivial[T]:Ordering[T]	=
			new Ordering[T] {
				def compare(x:T, y:T):Int	= 0
			}

		/** orders lexicographically from left to right */
		def sequence[T](missingFirst:Boolean)(using base:Ordering[T]):Ordering[Seq[T]]	=
			new Ordering[Seq[T]] {
				def compare(a:Seq[T], b:Seq[T]):Int	= {
					@tailrec
					def loop(index:Int):Int	=
						(a.lift(index), b.lift(index)) match {
							case (Some(aa), Some(bb))	=>
								val tmp	= base.compare(aa, bb)
								if (tmp != 0)	tmp
								else			loop(index+1)
							case (Some(aa), None)	=>
								if (missingFirst) +1 else -1
							case (None, Some(bb))	=>
								if (missingFirst) -1 else +1
							case (None, None)	=>
								0
						}
					loop(0)
				}
			}

		def many[T](orderings:Seq[Ordering[T]]):Ordering[T]	=
			orderings.foldLeft(Ordering.trivial[T])(_ `orElse` _)

		def manyVar[T](orderings:Ordering[T]*):Ordering[T]	=
			many(orderings.toVector)
	}

	extension [T](peer:Ordering[T]) {
		/** Ordering should be contravariant */
		def vary[U<:T]:Ordering[U]	=
			new Ordering[U] {
				def compare(x:U, y:U):Int	= {
					peer.compare(x, y)
				}
			}

		def contraMap[S](func:S=>T):Ordering[S]	=
			peer on func

		/**
		alternative to the implicit (Ordering[T],Ordering[T]) => Ordering[T] conversion in Ordering.Implicits
		and Comparator's thenCompare. forms a Monoid with Orderings#trivial
		*/
		def orElse[U<:T](that:Ordering[U]):Ordering[U]	=
			new Ordering[U] {
				def compare(x:U, y:U):Int	= {
					val	high	= peer.compare(x,y)
					if (high != 0)	high
					else			that.compare(x,y)
				}
			}
	}
}
