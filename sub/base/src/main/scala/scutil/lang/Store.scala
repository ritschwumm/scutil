package scutil.lang

import scutil.lang.tc._

object Store extends StoreInstances {
	def identity[T](t:T):Store[T,T]	=
			Store(t, t => t)
		
	def trivial[T](t:T):Store[T,Unit]	=
			Store((), _ => t)
}

final case class Store[C,V](get:V, put:V=>C) {
	/** aka extract */
	def container:C							= put(get)
	
	def modify(func:Endo[V]):C				= put(func(get))
	
	def modifyF[F[_]](func:FEndo[F,V])(implicit F:Functor[F]):F[C]	=
			(F map func(get))(put)
			
	def modifyState[X](func:State[V,X]):(C,X)	= {
		val (v2, side)	= func run get
		(put(v2), side)
	}
	
	def modifyStateT[F[_],X](func:StateT[F,V,X])(implicit F:Functor[F]):F[(C,X)]	=
			(F map (func run get)) { case (v, x) => (put(v), x) }
	
	//------------------------------------------------------------------------------
	
	/** aka duplicate */
	def coFlatten:Store[Store[C,V],V]	=
			Store(get, Store(_, put))
		
	def map[CC](func:C=>CC):Store[CC,V]	=
			Store[CC,V](
				get,
				put andThen func
			)
		
	def coFlatMap[CC](func:Store[C,V]=>CC):Store[CC,V] =
			Store[CC,V](
				get,
				x => func(Store(x, put))
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
				that.put andThen this.put
			)
		
	def compose[CC](that:Store[CC,C]):Store[CC,V]	=
			that andThen this
		
	// TODO optics cleanup
	def andThenBijection[U](that:Bijection[V,U]):Store[C,U]	=
			Store(
				that get get,
				that.put andThen put
			)
}

trait StoreInstances {
	implicit def StoreFunctor[S]:Functor[Store[?,S]]	=
			new Functor[Store[?,S]] {
				def map[A,B](it:Store[A,S])(func:A=>B):Store[B,S]	= it map func
			}
}
