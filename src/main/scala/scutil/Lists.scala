package scutil

import scala.annotation.tailrec

import scutil.lang._

object Lists {
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
