package scutil.lang

import java.util.concurrent.atomic.AtomicReference

object IoRef {
	def apply[T](initial:T):IoRef[T]	= new IoRef(initial)
}

final class IoRef[T](initial:T) {
	val ref	= new AtomicReference(initial)

	val get:Io[T]	=
			Io delay {
				ref.get
			}

	def set(it:T):Io[T]	=
			Io delay {
				ref getAndSet it
			}

	def update(func:Endo[T]):Io[T]	=
			modify { old =>
				func(old) -> old
			}

	def modify[U](func:T=>(T,U)):Io[U]	=
			Io delay {
				@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
				var out:U	= null.asInstanceOf[U]
				ref getAndUpdate { old =>
					val (next, res)	= func apply old
					out	= res
					next
				}
				out
			}

	def modifyInIo[U](func:Io[T=>(T,U)]):Io[U]	=
			modify { old =>
				func unsafeRun () apply old
			}

	def modifyToIo[U](func:T=>Io[(T,U)]):Io[U]	=
			modify { old =>
				func(old) unsafeRun ()
			}

	def modifyState[U](state:State[T,U]):Io[U] =
			modify(state.run)

	def modifyStateT[U](stateT:StateT[Io,T,U])	=
			modifyToIo(stateT.run)

	def modifyIoState[U](state:Io[State[T,U]]):Io[U] =
			modifyInIo(state map (_.run))

	override def toString:String	=
			s"IoRef(${get.unsafeRun()})"
}
