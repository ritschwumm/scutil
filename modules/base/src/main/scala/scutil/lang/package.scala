package scutil

package object lang {
	type Identity[T]		= T
	type Predicate[-T]		= T=>Boolean

	type Thunk[+T]			= ()=>T
	type Effect[-T]			= T=>Unit
	type Executor			= Effect[Thunk[Unit]]

	// Function[-S,+T]
	type PFunction[-S,+T]		= S=>Option[T]
	// NOTE this is ReaderT
	type FFunction[F[_],S,T]	= S=>F[T]

	type Endo[T]				= T=>T
	type PEndo[T]				= T=>Option[T]
	type FEndo[F[_],T]			= T=>F[T]

	//------------------------------------------------------------------------------

	def constant[S,T](value: =>T):(S=>T)		= _ => value
	def ignorant[S,T](thunk:Thunk[T]):(S=>T)	= _ => thunk()

	def thunk[T](value: =>T):Thunk[T]			= () => value

	def io[T](block: =>T):Io[T]					= Io delay block
	def disposable(block: =>Unit):Disposable	= Disposable delay block

	/** tell the compiler the control flow never reaches this point */
	def nothing:Nothing	= sys error "silence! i kill you!"

	//------------------------------------------------------------------------------

	type ~>[-F[_],+G[_]]	= NaturalTransformation[F,G]

	//------------------------------------------------------------------------------

	/** type inference helper */
	def taking[S]	= new FunctionTaking[S]

	def typed[T](t : => T):Unit	= {}
}
