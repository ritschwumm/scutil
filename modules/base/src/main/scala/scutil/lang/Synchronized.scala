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
		modify(State setOld value)

	/** change state and return something */
	def modify[U](state:State[T,U]):U	=
		synchronized {
			val (next, out)	= state run value
			value	= next
			out
		}

	/** change state only */
	def update(func:Endo[T]):Unit	=
		modify(State mod func)

	override def toString:String	=
		s"Synchronized(${get().toString})"
}
