package scutil.lang

object Store {
	def identity[T](t:T):Store[T,T]	= 
			Store(t, t => t)
		
	def trivial[T](t:T):Store[T,Unit]	=
			Store((), _ => t)
}

final case class Store[C,V](get:V, put:V=>C) {
	/** aka extract */
	def container:C							= put(get)
	
	def modify(func:Endo[V]):C				= put(func(get))
	def modifyOpt(func:PEndo[V]):Option[C]	= func(get) map put
	
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
			
	def traverseISeq[B](implicit ev:V=>ISeq[B]):ISeq[Store[C,B]]	=
			(0 until get.size).toVector flatMap { idx =>
				// NOTE asInstanceOf is justified by the implicit evidence
				PLenses.iseq[B](idx) overTotal this.asInstanceOf[Store[C,ISeq[B]]]
			}
}
