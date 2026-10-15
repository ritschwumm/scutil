package scutil.lang

object Delay {
	def apply[F[_]:Delay as D]:Delay[F]	= D

	def of[F[_]:Delay as D, T](it: =>T):F[T]			= D.delay(it)
	def ofThunk[F[_]:Delay as D, T](it:Thunk[T]):F[T]	= D.delayThunk(it)
}

trait Delay[F[_]] {
	def delay[T](it: =>T):F[T]

	def delayThunk[T](it:Thunk[T]):F[T]	= delay(it())
}
