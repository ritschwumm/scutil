package scutil.collection.extension

import scala.annotation.tailrec

object ListExtensions {
	extension [T](peer:List[T]) {
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
			def loop(prefix:List[U], list1:List[U], list2:List[U]):(List[U],List[U],List[U])	= {
				(list1, list2) match {
					case (h1 :: t1, h2 :: t2) if h1 == h2	=> loop(h1 :: prefix, t1, t2)
					case _									=> (prefix.reverse, list1, list2)
				}
			}
			loop(Nil, peer, other)
		}
	}
}
