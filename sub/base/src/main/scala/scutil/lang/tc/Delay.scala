package scutil.lang.tc

import scutil.lang._

object Delay {
	def apply[F[_]](implicit ev:Delay[F]):Delay[F]	= ev
	
	def of[F[_],T](it: =>T)(implicit ev:Delay[F]):F[T]			= ev delay it
	def ofThunk[F[_],T](it:Thunk[T])(implicit ev:Delay[F]):F[T]	= ev delayThunk it
}

trait Delay[F[_]] {
	// implement at least one of these
	def delay[T](it: =>T):F[T]			= delayThunk(() => it)
	def delayThunk[T](it:Thunk[T]):F[T]	= delay(it())
}
