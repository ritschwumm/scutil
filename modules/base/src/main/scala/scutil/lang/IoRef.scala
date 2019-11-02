package scutil.lang

import java.util.concurrent.atomic.AtomicReference

object IoRef {
	// NOTE initial could be lazy here
	def apply[T](initial:T):Io[IoRef[T]]	=
			Io delay new IoRef(initial)
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
				RefUtil modify (ref, func)
			}

	def modifyState[U](state:State[T,U]):Io[U] =
			modify(state.run)

	/*
	// NOTE this does not work in scala-js because getAndUpdate is not implemented
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
	*/

	//------------------------------------------------------------------------------

	override def toString:String	=
			"IoRef(...)"
}
