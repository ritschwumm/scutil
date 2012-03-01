package scutil

object Functions {
	/*
	// BETTER end in colon so it associates to the right
	type =>?[-S,+T]	= PartialFunction[S,T]
	type ?<=>[S,T]	= Marshaller[S,T]
	type <=>[S,T]	= Bijection[S,T]
	*/
	
	type Predicate[-T]	= T=>Boolean
	
	type Thunk[+T]		= ()=>T			// aka Function0[T] aka Future[T] see Callable
	type Effect[-T]		= T=>Unit		// aka Function1[T,Unit] aka Cont[T]
	
	type Task			= Thunk[Unit]	// see Runnable
	type Executor		= Task=>Unit
	
	//------------------------------------------------------------------------------
	
	def constant[S,T](value: =>T):(S=>T)		= _ => value
	def ignorant[S,T](thunk:Thunk[T]):(S=>T)	= _ => thunk()
	def task(value: =>Unit):Task				= thunk(value)
	def thunk[T](value: =>T):Thunk[T]			= () => value
	
	/** used to show the reader control flow never reaches this point */
	def neverComesHere:Nothing	= sys error "silence! i kill you!"
}
