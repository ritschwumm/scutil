package scutil

package object lang {
	// use this instead of scala.collection.Seq provided in Predef
	type ISeq[+T]	= scala.collection.immutable.Seq[T]
	val ISeq		= scala.collection.immutable.Seq
	
	//------------------------------------------------------------------------------
	
	type Identity[T]		= T
	
	type Endo[T]			= T=>T
	type Predicate[-T]		= T=>Boolean
	
	type Thunk[+T]			= ()=>T
	type Effect[-T]			= T=>Unit
	
	type Task				= Thunk[Unit]
	type Executor			= Effect[Task]
	
	type Stateful[T,+X]		= T=>(T,X)
	
	type PFunction[-S,+T]	= S=>Option[T]
	type PEndo[T]			= T=>Option[T]
	type PStateful[T,+X]	= T=>Option[(T,X)]
	
	type FFunction[F[_],S,T]	= S=>F[T]
	type FEndo[F[_],T]			= T=>F[T]
	type FStateful[F[_],T,X]	= T=>F[(T,X)]
	
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
	
	def typed[T](t : => T) {}
}
