package scutil.collection.pimp

import scala.annotation.tailrec

import scutil.lang._

object ListImplicits extends ListImplicits

trait ListImplicits {
	implicit final class ListCompanionExt(peer:List.type) {
		def unfoldRight[S,T](seed:S, func:PFunction[S,(S,T)]):List[T]	=
				func(seed) match {
					case Some((next,out))	=> out :: unfoldRight(next, func)
					case None				=> Nil
				}

		// == unfoldRight(seed, func andThen { it => (it,it) })
		def unfoldRightSimple[S,T<:S](seed:S, func:PFunction[S,T]):List[T]	=
				func(seed) match {
					case Some(next)	=> next :: unfoldRightSimple(next, func)
					case None		=> Nil
				}

		def unfoldLeft[S,T](seed:S, func:PFunction[S,(S,T)]) = {
			@tailrec
			def recurse(seed:S, accu:List[T]):List[T] = func(seed) match {
				case Some((next,out))	=> recurse(next, out :: accu)
				case None				=> accu
			}
			recurse(seed, Nil)
		}

		// == unfoldLeft(seed, func andThen { it => (it,it) })
		def unfoldLeftSimple[S,T<:S](seed:S, func:PFunction[S,T]) = {
			@tailrec
			def recurse(seed:S, accu:List[T]):List[T] = func(seed) match {
				case Some(next)	=> recurse(next, next :: accu)
				case None		=> accu
			}
			recurse(seed, Nil)
		}
	}

	implicit final class ListExt[T](peer:List[T]) {
		def cata[U](nil: =>U, cons:(T,List[T])=>U):U =
				peer match {
					case head :: tail	=> cons(head, tail)
					case Nil			=> nil
				}

		/**
		calculate common prefix and differing tails for two lists
		usage example:
		<code>
		List(1,2,3,4) unprefix List(1,2,4,5)
		==&gt; Triple(List(1,2), List(3,4), List(4,5))
		</code>
		*/
		def unprefix[U>:T](other:List[U]):(List[U],List[U],List[U])	= {
			@tailrec
			def loop[V](prefix:List[V], list1:List[V], list2:List[V]):(List[V],List[V],List[V])	= {
				(list1, list2) match {
					case (h1 :: t1, h2 :: t2) if h1 == h2	=> loop(h1 :: prefix, t1, t2)
					case _									=> (prefix.reverse, list1, list2)
				}
			}
			loop(Nil, peer, other)
		}
	}
}
