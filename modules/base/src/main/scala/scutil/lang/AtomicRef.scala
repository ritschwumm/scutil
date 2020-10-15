package scutil.lang

import java.util.concurrent.atomic.AtomicReference

import scutil.lang.tc._

object AtomicRef {
	// NOTE initial could be lazy here
	def apply[F[_]:Delay,G[_]:Delay,T](initial:T):F[AtomicRef[G,T]]	=
		Delay[F] delay new AtomicRef(initial)
}

final class AtomicRef[F[_]:Delay,T](initial:T) {
	private val ref	= new AtomicReference(initial)

	private val D	= Delay[F]

	val get:F[T]	=
		D delay {
			ref.get
		}

	def set(it:T):F[T]	=
		D delay {
			ref getAndSet it
		}

	def update(func:Endo[T]):F[T]	=
		modify { old =>
			func(old) -> old
		}

	def modify[U](func:T=>(T,U)):F[U]	=
		D delay {
			RefUtil.modify(ref, func)
		}

	def modifyState[U](state:State[T,U]):F[U] =
		modify(state.run)

	/*
	// NOTE this does not work in scala-js because getAndUpdate is not implemented
	def modify[U](func:T=>(T,U)):F[U]	=
		D delay {
			@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
			var out:U	= null.asInstanceOf[U]
			ref getAndUpdate { old =>
				val (next, res)	= func apply old
				out	= res
				next
			}
			out
		}
	*/

	// TODO bullshit
	override def toString:String	=
		"AtomicRef(...)"
}