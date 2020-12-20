package scutil.lang

object Synchronized {
	def apply[T](initial:T):Synchronized[T]	=
		new Synchronized[T](initial)
}

final class Synchronized[T](initial:T) {
	var value	= initial

	def get():T	=
		synchronized {
			value
		}

	/** returns the previous value */
	def set(value:T):T	=
		modify { old => (value, old) }

	/** returns the previous value */
	def update(func:T=>T):T	=
		modify { old => (func(value), old) }

	/** change state and return something */
	def modify[U](func:T=>(T,U)):U	=
		synchronized {
			val (next, out)	= func(value)
			value	= next
			out
		}

	/** change state and return something */
	def modifyState[U](state:State[T,U]):U	=
		modify(state.run)

	override def toString:String	=
		s"Synchronized(${get().toString})"
}
