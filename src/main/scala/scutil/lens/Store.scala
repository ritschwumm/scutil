package scutil.lens

import scutil.lang._
import scutil.Implicits._

object Store {
	def identity[T](t:T):Store[T,T]	= 
			Store(t, t => t)
		
	def trivial[T](t:T):Store[T,Unit]	=
			Store((), _ => t)
}

final case class Store[C,V](get:V, put:V=>C) {
	def container:C	= put(get)
	
	def modify(func:Endo[V]):C	= put(func(get))
	
	def map[CC](func:C=>CC):Store[CC,V]	=
			Store[CC,V](get, put andThen func)
		
	def coFlatMap[CC](func:Store[C,V]=>CC):Store[CC,V] =
			Store[CC,V](get, x => func(Store(x, put)))
		
	def coFlatten[CC](implicit ev:Store[C,V]=>CC):Store[CC,V] =
			coFlatMap(ev)
		
	def xmapValue[U](bijection:Bijection[V,U]):Store[C,U]	=
			Store(bijection write get, bijection.read andThen put)
		
	def xmapValueInverse[U](bijection:Bijection[U,V]):Store[C,U]	=
			xmapValue(bijection.inverse)
		
	/** symbolic alias for andThen */
	@inline
	def >=>[VV](that:Store[V,VV]):Store[C,VV]	=
			this andThen that
		
	/** symbolic alias for compose */
	@inline
	def <=<[CC](that:Store[CC,C]):Store[CC,V]	=
			this compose that
		
	def andThen[VV](that:Store[V,VV]):Store[C,VV]	=
			Store(that.get, that.put andThen this.put)
		
	def compose[CC](that:Store[CC,C]):Store[CC,V]	=
			that andThen this
}
