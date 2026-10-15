package scutil.lang

import scutil.lang.implicits.*

object Setters {
	//-----------------------------------------------------------------------------
	//## tuple

	def first[S, T]:Setter[(S, T), S]	=
		Setter { ss => st =>
			(ss(st._1), st._2)
		}

	def second[S, T]:Setter[(S, T), T]	=
		Setter { tt => st =>
			(st._1, tt(st._2))
		}

	//-----------------------------------------------------------------------------
	//## Option

	def some[T]:Setter[Option[T], T]	=
		Setter { tt => to =>
			to.map(tt)
		}

	//-----------------------------------------------------------------------------
	//## Either

	def left[L, R]:Setter[Either[L, R], L]	=
		Setter { ll => lr =>
			lr.leftMap(ll)
		}

	def right[L, R]:Setter[Either[L, R], R]	=
		Setter { rr => lr =>
			lr.map(rr)
		}
}
