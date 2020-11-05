package scutil.diff

object DiffDirection {
	case object Neither		extends DiffDirection
	case object Up			extends DiffDirection
	case object Left		extends DiffDirection
	case object UpAndLeft	extends DiffDirection
}

sealed trait DiffDirection
