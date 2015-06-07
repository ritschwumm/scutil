package scutil.lang

object Synchronized {
	def apply[T](initial:T):Synchronized[T]	=
			new Synchronized[T](initial)
}

final class Synchronized[T](initial:T) {
	var value	= initial
	
	def get:T	=
			synchronized {
				value
			}
		
	/** returns the previous value */
	def set(value:T):T	=
			modify((value, _))
			
	/** change state and return something */
	def modify[U](func:T=>(T,U)):U	=
			synchronized {
				val (next, out)	= func(value)
				value	= next
				out
			}
			
	/** change state only */
	def update(func:Endo[T]):Unit	=
			modify { it => (func(it), ()) }
			
	override def toString:String	=
			s"Synchronized(${get})"
}
