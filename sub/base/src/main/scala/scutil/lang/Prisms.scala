package scutil.lang

object Prisms {
	def eitherLeft[A,B]:Prism[Either[A,B],A]		= Prism(_.left.toOption,	Left.apply)
	def eitherRight[A,B]:Prism[Either[A,B],B]		= Prism(_.right.toOption,	Right.apply)
	
	def validatedBad[A,B]:Prism[Validated[A,B],A]	= Prism(_.badOption,		Bad.apply)
	def validatedGood[A,B]:Prism[Validated[A,B],B]	= Prism(_.toOption,			Good.apply)
	
	def listCons[T]:Prism[List[T],(T,List[T])]	=
			Prism(
				_ match {
					case h :: t		=> Some((h, t))
					case Nil		=> None
				},
				ht => ht._1 :: ht._2
			)
	
	def listNil[T]:Prism[List[T],Unit]	=
			Prism(
				_ match {
					case Nil	=> Some(())
					case _		=> None
				},
				_ => Nil
			)
			
	// @see extractHead
	def iseqHead[T]:Prism[ISeq[T],(T,ISeq[T])]	=
			Prism(
				_ match {
					case h +: t	=> Some((h, t))
					case _		=> None
				},
				ht => ht._1 +: ht._2
			)
			
	// @see extractLast
	def iseqLast[T]:Prism[ISeq[T],(ISeq[T],T)]	=
			Prism(
				_ match {
					case i :+ l	=> Some((i, l))
					case _		=> None
				},
				il => il._1 :+ il._2
			)
	
	def iseqEmpty[T]:Prism[ISeq[T],Unit]	=
			Prism(
				_ match {
					case ISeq()	=> Some(())
					case _		=> None
				},
				_ => Vector.empty
			)
}
