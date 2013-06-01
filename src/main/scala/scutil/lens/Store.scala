package scutil.lens

import scutil.lang._
import scutil.Implicits._

object Store {
	def identity[T](t:T):Store[T,T]	= 
			Store(t, t => t)
		
	def trivial[T](t:T):Store[T,Unit]	=
			Store((), _ => t)
}

case class Store[C,V](get:V, set:V=>C) {
	def mod(func:Endo[V]):C	= set(func(get))
	
	def map[CC](func:C=>CC):Store[CC,V]	=
			Store[CC,V](get, set andThen func)
		
	def coFlatMap[CC](func:Store[C,V]=>CC):Store[CC,V] =
			Store[CC,V](get, x => func(Store(x, set)))
		
	def coFlatten[CC](implicit ev:Store[C,V]=>CC):Store[CC,V] =
			coFlatMap(ev)
		
	def xmapValue[U](bijection:Bijection[V,U]):Store[C,U]	=
			Store(bijection write get, bijection.read _ andThen set)
		
	def andThen[VV](that:Store[V,VV]):Store[C,VV]	=
			Store(that.get, that.set andThen this.set)
		
	def compose[CC](that:Store[CC,C]):Store[CC,V]	=
			that andThen this
}
