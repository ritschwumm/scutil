package scutil

package object lang {
	type Endo[T]		= T=>T
	type Predicate[-T]	= T=>Boolean
	
	type Thunk[+T]		= ()=>T
	type Effect[-T]		= T=>Unit
	
	type Task			= Thunk[Unit]
	type Executor		= Effect[Task]
	
	type Chance[-S,+T]	= S=>Option[T]
	
	//------------------------------------------------------------------------------
	
	def constant[S,T](value: =>T):(S=>T)		= _ => value
	def ignorant[S,T](thunk:Thunk[T]):(S=>T)	= _ => thunk()
	
	def task(value: =>Unit):Task				= thunk(value)
	def thunk[T](value: =>T):Thunk[T]			= () => value
	
	/** tell the compiler the control flow never reaches this point */
	def neverComesHere:Nothing	= sys error "silence! i kill you!"
}
