package scutil.lang

import scutil.lang.tc._
import scutil.lang.tc.syntax._

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
	
	def modifyState[X](func:State[V,X]):(C,X)	= {
		val (v2, side)	= func run get
		(put(v2), side)
	}
	
	//------------------------------------------------------------------------------
	
	def modifyF[F[_]:Functor](func:FEndo[F,V]):F[C]	=
			func(get) map put
			//(Functor[F] map func(get))(put)
		
	/*
	def modifyStatefulF[F[_]:Functor,X](func:FStateful[F,V,X]):F[(C,X)]	=
			func(get) map { case (v, x) => (put(v), x) }
			//(Functor[F] map func(get)) { case (v,x) => (put(v), x) }
	*/
	
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
		
	def andThenBijection[U](that:Bijection[V,U]):Store[C,U]	=
			Store(
				that write get,
				that.read andThen put
			)
}

trait StoreInstances {
	implicit def StoreFunctor[S]:Functor[ ({type l[T]=Store[T,S]})#l ]	=
			new Functor[ ({type l[T]=Store[T,S]})#l ] {
				def map[A,B](it:Store[A,S])(func:A=>B):Store[B,S]				= it map func
			}
}
