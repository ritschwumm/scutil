package scutil.lang

import java.util.concurrent.atomic.AtomicReference

// TODO cats get rid of this
object AtomicRef {
	// NOTE initial could be lazy here
	def apply[F[_]:Delay,G[_]:Delay,T](initial:T):F[AtomicRef[G,T]]	=
		Delay[F] delay new AtomicRef(initial)

	def simple[F[_]:Delay,T](initial:T):F[AtomicRef[F,T]]	=
		apply[F,F,T](initial)

	// TODO isn't this just a polymorphic contextual function?

	def builder[G[_]:Delay,T](initial:T)	= new Builder(initial)

	final class Builder[G[_]:Delay,T](initial:T) {
		def build[F[_]:Delay]	= apply[F,G,T](initial)
	}
}

final class AtomicRef[F[_]:Delay,T](initial:T) {
	private val ref	= new AtomicReference(initial)

	private val D	= Delay[F]

	val get:F[T]	=
		D delay {
			ref.get
		}

	/** returns the previous value */
	def set(it:T):F[T]	=
		D delay {
			ref getAndSet it
		}

	/** returns the previous value */
	def update(func:T=>T):F[T]	=
		modify { old =>
			func(old) -> old
		}

	def modify[U](func:T=>(T,U)):F[U]	=
		D delay {
			RefUtil.modify(ref, func)
		}

	def modifyState[U](state:State[T,U]):F[U] =
		modify(state.run)

	// TODO bullshit
	override def toString:String	=
		"AtomicRef(...)"
}
