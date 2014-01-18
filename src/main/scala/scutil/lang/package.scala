package scutil

package object lang {
	type Endo[T]			= T=>T
	type Predicate[-T]		= T=>Boolean
	
	type Thunk[+T]			= ()=>T
	type Effect[-T]			= T=>Unit
	
	type Task				= Thunk[Unit]
	type Executor			= Effect[Task]
	
	type PFunction[-S,+T]	= S=>Option[T]
	type PEndo[T]			= T=>Option[T]
	
	//------------------------------------------------------------------------------
	
	def constant[S,T](value: =>T):(S=>T)		= _ => value
	def ignorant[S,T](thunk:Thunk[T]):(S=>T)	= _ => thunk()
	
	def task(block: =>Unit):Task				= thunk(block)
	def thunk[T](value: =>T):Thunk[T]			= () => value
	
	def disposable(block: =>Unit):Disposable	= Disposable(task(block))
	
	/** tell the compiler the control flow never reaches this point */
	def nothing:Nothing	= sys error "silence! i kill you!"
	
	//------------------------------------------------------------------------------
	
	/** type inference helper */
	def taking[S]	= new FunctionTaking[S]
	
	final class FunctionTaking[S] {
		def apply[T](func:S=>T):S=>T	= func
	} 
}
