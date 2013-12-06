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
	def set(value:T):Unit	=
			synchronized { 
				val old	= this.value
				this.value  = value
				old
			}
		
	/** returns the previous value */
	def mod(func:T=>T):T	=
			synchronized { 
				val old	= value
				value  = func(value)
				old
			}
			
	override def toString:String	=
			s"Synchronized(${get})"
}
