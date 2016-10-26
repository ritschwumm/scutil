package scutil.lang

/** aka Semigroup */
object CanConcat {
	def apply[T:CanConcat]:CanConcat[T]	= implicitly[CanConcat[T]]
	
	def by[T](func:(T,T)=>T):CanConcat[T]	=
			new CanConcat[T] {
				def concat(a:T, b:T):T	= func(a, b)
			}
			
	//------------------------------------------------------------------------------
	
	implicit def ISeqCanConcat[T]:CanConcat[ISeq[T]]	=
			CanConcat by (_ ++ _)
			
	implicit def VectorCanConcat[T]:CanConcat[Vector[T]]	=
			CanConcat by (_ ++ _)
	
	implicit def ListCanConcat[T]:CanConcat[List[T]]	=
			CanConcat by (_ ++ _)
	
	implicit def NesCanConcat[T]:CanConcat[Nes[T]]	=
			CanConcat by (_ ++ _)
		
	implicit def SetCanConcat[T]:CanConcat[Set[T]]	=
			CanConcat by (_ ++ _)
		
	implicit def MapCanConcat[K,V]:CanConcat[Map[K,V]]	=
			CanConcat by (_ ++ _)
}

trait CanConcat[T] {
	def concat(a:T, b:T):T
}
