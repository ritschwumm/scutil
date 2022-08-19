package scutil.gui.geom

object SgOrientation {
	def trueHorizontal(horizontal:Boolean):SgOrientation	=
		if (horizontal)	SgOrientation.Horizontal
		else			SgOrientation.Vertical

	def trueVertical(vertical:Boolean):SgOrientation	=
		if (vertical)	SgOrientation.Vertical
		else			SgOrientation.Horizontal
}

enum SgOrientation {
	case Horizontal
	case Vertical

	def cata[T](horizontal: =>T, vertical: =>T):T	=
		this match {
			case SgOrientation.Horizontal	=> horizontal
			case SgOrientation.Vertical		=> vertical
		}

	def opposite:SgOrientation	=
		this match {
			case SgOrientation.Horizontal	=> SgOrientation.Vertical
			case SgOrientation.Vertical		=> SgOrientation.Horizontal
		}
}
