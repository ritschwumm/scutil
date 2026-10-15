package scutil.lang

object Prisms {
	//-----------------------------------------------------------------------------
	//## Option

	def some[T]:Prism[Option[T],T]			= Prism(x=>x, Some.apply)

	//-----------------------------------------------------------------------------
	//## Either

	def left[A,B]:Prism[Either[A,B],A]		= Prism(_.swap.toOption,	Left.apply)
	def right[A,B]:Prism[Either[A,B],B]		= Prism(_.toOption,			Right.apply)

	//-----------------------------------------------------------------------------
	//## Validated

	def invalid[A,B]:Prism[Validated[A,B],A]	= Prism(_.invalidToOption,	Validated.invalid)
	def valid[A,B]:Prism[Validated[A,B],B]		= Prism(_.toOption,			Validated.valid)

	//-----------------------------------------------------------------------------
	//## List

	def listCons[T]:Prism[List[T], (T, List[T])]	=
		Prism(
			get	=
				_ match {
					case h :: t		=> Some((h, t))
					case Nil		=> None
				},
			set	=
				ht => ht._1 :: ht._2
		)

	def listNil[T]:Prism[List[T], Unit]	=
		Prism(
			get	=
				_ match {
					case Nil	=> Some(())
					case _		=> None
				},
			set	=
				_ => Nil
		)

	//-----------------------------------------------------------------------------
	//## Seq

	// @see extractHead
	def seqHead[T]:Prism[Seq[T], (T, Seq[T])]	=
		Prism(
			get	=
				_ match {
					case h +: t	=> Some((h, t))
					case _		=> None
				},
			set	=
				ht => ht._1 +: ht._2
		)

	// @see extractLast
	def seqLast[T]:Prism[Seq[T], (Seq[T], T)]	=
		Prism(
			get	=
				_ match {
					case i :+ l	=> Some((i, l))
					case _		=> None
				},
			set	=
				il => il._1 :+ il._2
		)

	def seqEmpty[T]:Prism[Seq[T], Unit]	=
		Prism(
			get	=
				_ match {
					case Seq()	=> Some(())
					case _		=> None
				},
			set	=
				_ => Vector.empty
		)

	//-----------------------------------------------------------------------------
	//## Vector

	def vectorHead[T]:Prism[Vector[T], (T, Vector[T])]	=
		Prism(
			get	=
				_ match {
					case h +: t	=> Some((h, t))
					case _		=> None
				},
			set	=
				ht => ht._1 +: ht._2
		)

	def vectorLast[T]:Prism[Vector[T], (Vector[T], T)]	=
		Prism(
			get	=
				_ match {
					case i :+ l	=> Some((i, l))
					case _		=> None
				},
			set	=
				il => il._1 :+ il._2
		)

	def vectorEmpty[T]:Prism[Vector[T], Unit]	=
		Prism(
			get	=
				_ match {
					case Seq()	=> Some(())
					case _		=> None
				},
			set	=
				_ => Vector.empty
		)
}
