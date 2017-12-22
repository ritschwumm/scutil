package scutil.lang

import scutil.lang.tc._

object Store extends StoreInstances {
	def identity[T](t:T):Store[T,T]	=
			Store(t, t => t)
		
	def trivial[T](t:T):Store[T,Unit]	=
			Store((), _ => t)
}

final case class Store[C,V](get:V, set:V=>C) {
	/** aka extract */
	def container:C							= set(get)
	
	def modify(func:Endo[V]):C				= set(func(get))
	
	def modifyF[F[_]](func:FEndo[F,V])(implicit F:Functor[F]):F[C]	=
			(F map func(get))(set)
			
	def modifyState[X](func:State[V,X]):(C,X)	= {
		val (v2, side)	= func run get
		(set(v2), side)
	}
	
	def modifyStateT[F[_],X](func:StateT[F,V,X])(implicit F:Functor[F]):F[(C,X)]	=
			(F map (func run get)) { case (v, x) => (set(v), x) }
	
	//------------------------------------------------------------------------------
	
	/** aka duplicate */
	def coFlatten:Store[Store[C,V],V]	=
			Store(get, Store(_, set))
		
	def map[CC](func:C=>CC):Store[CC,V]	=
			Store[CC,V](
				get,
				set andThen func
			)
		
	def coFlatMap[CC](func:Store[C,V]=>CC):Store[CC,V] =
			Store[CC,V](
				get,
				x => func(Store(x, set))
			)
		
	/** symbolic alias for andThen */
	def >=>[VV](that:Store[V,VV]):Store[C,VV]	=
			this andThen that
		
	/** symbolic alias for compose */
	def <=<[CC](that:Store[CC,C]):Store[CC,V]	=
			this compose that
		
	def andThen[VV](that:Store[V,VV]):Store[C,VV]	=
			Store(
				that.get,
				that.set andThen this.set
			)
		
	def compose[CC](that:Store[CC,C]):Store[CC,V]	=
			that andThen this
		
	// TODO optics cleanup
	def andThenBijection[U](that:Bijection[V,U]):Store[C,U]	=
			Store(
				that get get,
				that.set andThen set
			)
}

trait StoreInstances {
	implicit def StoreFunctor[S]:Functor[Store[?,S]]	=
			new Functor[Store[?,S]] {
				def map[A,B](it:Store[A,S])(func:A=>B):Store[B,S]	= it map func
			}
}
