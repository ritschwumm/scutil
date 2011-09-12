package scutil.validation

object NonEmptyList {
	def unital[T](value:T):NonEmptyList[T]	= End(value)
	
	// TODO speedup
	def apply[T](value1:T, values:T*):NonEmptyList[T]	=
			option(values) match {
				case Some(values)	=> Mid(value1, values)
				case None			=> End(value1)
			}
		
	// TODO speedup
	def option[T](value:Seq[T]):Option[NonEmptyList[T]]	= value.size match {
		case 0	=> None
		case 1	=> Some(End(value.head))
		case _	=> option(value.tail) map { value.head :: _ }
	}
}

/** 
The Non-Empty List is quite like a standard single linked List, but always contains at least one element.
Concatenation of NonEmptyLists is a Semigroup, but not a Monoid because there is no neutral element.
*/
sealed abstract class NonEmptyList[+T] {
	/** prepend an element. fast */
	def prepend[U>:T](value:U):NonEmptyList[U]	= value +: this
	/** alias for prepend */
	def +: [U>:T](value:U):NonEmptyList[U]		= value :: this
	
	/** append an element. slow */
	def append [U>:T](value:U):NonEmptyList[U]	= value +: this
	/** alias for append */
	def :+ [U>:T](value:U):NonEmptyList[U]	= cata (
			(value1, next1)	=> value1 :: (next1 :+ value),
			(value1)		=> value1 :: End(value))
	
	/** cons an element to this NonEmptyList. */
	def :: [U>:T](value:U):NonEmptyList[U]	= Mid(value, this)
	/** concatenate two NonEmptyLists. slow. */
	def ++ [U>:T](that:NonEmptyList[U]):NonEmptyList[U]	= cata(
			(value1, next1)	=> value1 :: (next1 ++ that),
			(value1)		=> value1 :: that)
	
	//------------------------------------------------------------------------------
	
	/** catamorphism used for pattern matching  */
	def cata[U](midFunc:(T,NonEmptyList[T])=>U, endFunc:T=>U):U
	
	def fold[U](last:U, func:(T,U)=>U):U	= cata(
			(value, next)	=> func(value, next fold (last,func)),
			(value)			=> func(value, last))
	
	def foreach(func:T=>Unit):Unit	= cata(
			(head, next)	=> { func(head); next foreach func },
			(head)			=> func(head))
	
	def map[U](func:T=>U):NonEmptyList[U]	= cata(
			(value, next)	=> Mid(func(value), next map func),
			(value)			=> End(func(value)))
	
	def flatMap[U](func:T=>NonEmptyList[U]):NonEmptyList[U]	= cata(
			(value, next)	=> func(value) ++ (next flatMap func),
			(value)			=> func(value))
	
	// TODO implement ap using flatMap?
	// def ap[U,V](that:NonEmptyList[U])(implicit witness:T=>U=>V):NonEmptyList[V] = (this, that) match {
	// 	case (n1@Mid(value1, next1),	n2@Mid(value2, next2))	=>
	// 	case (n1@Mid(value1, next1),	n2@End(value2))			=>
	// 	case (n1@End(value1),			n2@Mid(value2, next2))	=>
	// 	case (n1@End(value1),			n2@End(value2))			=>
	// }
	
	//------------------------------------------------------------------------------
	
	/** because we are non-empty there always is a head element */
	def head:T	= cata(
			(value, _)	=> value,
			identity)
	
	/** our tail may be empty */
	def tailOption:Option[NonEmptyList[T]]	= cata(
			(_, next)	=> Some(next),                
			(_)			=> None)
	
	/** convert to a standard List of at least one element */
	def toList:List[T]	= cata(
			(value, next)	=> value :: next.toList,
			(value)			=> List(value))
}

/** this NonEmptyList contains more than one element */
case class Mid[T](value:T, next:NonEmptyList[T])	extends NonEmptyList[T] {
	def cata[U](midFunc:(T,NonEmptyList[T])=>U, endFunc:T=>U):U	= midFunc(value, next)
}

/** this NonEmptyList contains exactly one element */
case class End[T](value:T) extends NonEmptyList[T] {
	def cata[U](midFunc:(T,NonEmptyList[T])=>U, endFunc:T=>U):U	= endFunc(value)
}
