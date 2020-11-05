package scutil

package object lang {
	type Identity[T]		= T
	type Predicate[-T]		= T=>Boolean

	type Thunk[+T]			= ()=>T
	type Effect[-T]			= T=>Unit
	@deprecated("use scutil.concurrent.Execution", "0.188.0")
	type Executor			= Effect[Thunk[Unit]]

	@deprecated("use S=>Option[T]", "0.188.0")
	type PFunction[-S,+T]		= S=>Option[T]
	// NOTE this is ReaderT
	@deprecated("use S=>F[T]", "0.188.0")
	type FFunction[F[_],S,T]	= S=>F[T]

	@deprecated("use T=>T", "0.188.0")
	type Endo[T]				= T=>T
	@deprecated("use T=>Option[T]", "0.188.0")
	type PEndo[T]				= T=>Option[T]
	@deprecated("use T=>F[T]", "0.188.0")
	type FEndo[F[_],T]			= T=>F[T]

	//------------------------------------------------------------------------------

	def constant[S,T](value: =>T):(S=>T)		= _ => value
	def ignorant[S,T](thunk:Thunk[T]):(S=>T)	= _ => thunk()

	def thunk[T](value: =>T):Thunk[T]			= () => value

	/** tell the compiler the control flow never reaches this point */
	def nothing:Nothing	= sys error "silence! i kill you!"

	//------------------------------------------------------------------------------

	type ~>[-F[_],+G[_]]	= NaturalTransformation[F,G]

	//------------------------------------------------------------------------------

	def typed[T](t : => T):Unit	= {}
}
