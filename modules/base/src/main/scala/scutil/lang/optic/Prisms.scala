package scutil.lang

object Prisms {
	def eitherLeft[A,B]:Prism[Either[A,B],A]		= Prism(_.swap.toOption,	Left.apply)
	def eitherRight[A,B]:Prism[Either[A,B],B]		= Prism(_.toOption,			Right.apply)

	def validatedBad[A,B]:Prism[Validated[A,B],A]	= Prism(_.badToOption,		Bad.apply)
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
	def seqHead[T]:Prism[Seq[T],(T,Seq[T])]	=
		Prism(
			_ match {
				case h +: t	=> Some((h, t))
				case _		=> None
			},
			ht => ht._1 +: ht._2
		)

	// @see extractLast
	def seqLast[T]:Prism[Seq[T],(Seq[T],T)]	=
		Prism(
			_ match {
				case i :+ l	=> Some((i, l))
				case _		=> None
			},
			il => il._1 :+ il._2
		)

	def seqEmpty[T]:Prism[Seq[T],Unit]	=
		Prism(
			_ match {
				case Seq()	=> Some(())
				case _		=> None
			},
			_ => Vector.empty
		)
}
