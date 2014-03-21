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
			synchronized { 
				val old	= this.value
				this.value  = value
				old
			}
		
	/** returns the previous and the current value */
	def mod(func:T=>T):(T,T)	=
			synchronized { 
				val old	= value
				value  = func(value)
				(old, value)
			}
			
	/** change state and return something */
	def chg[U](func:T=>(T,U)):U	=
			synchronized { 
				val (next, out)	= func(value)
				value	= next
				out
			}
			
	override def toString:String	=
			s"Synchronized(${get})"
}
