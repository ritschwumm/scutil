package scutil

package object lang {
	type Identity[T]		= T
	type Predicate[-T]		= T=>Boolean

	type Thunk[+T]			= ()=>T
	type Effect[-T]			= T=>Unit

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
