package scutil

object Types {
	type =>?[-S,+T]	= PartialFunction[S,T]
	type ?<=>[S,T]	= Marshaller[S,T]
	type <=>[S,T]	= Bijection[S,T]
	
	type Task			= Thunk[Unit]
	type Thunk[+T]		= ()=>T		// aka Function0[T] aka Future[T]
	type Effect[-T]		= T=>Unit	// Function1[T,Unit] aka Cont[T]
	type Executor		= Task=>Unit
	
	type Predicate[-T]	= T=>Boolean
}
