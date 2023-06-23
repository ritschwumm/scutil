package scutil.lang

import scala.deriving.*
import scala.compiletime.*

object FieldNames {
	inline given of[T](using m:Mirror.ProductOf[T]):FieldNames[T]	=
		FieldNames(showCase(m).toVector)

	private inline def showCase[T](m:Mirror.ProductOf[T]):List[String] =
		showElems[m.MirroredElemLabels]

	private inline def showElems[T <: Tuple]:List[String] =
		inline erasedValue[T] match {
			case _: (h *: t)	=> constValue[h & String] :: showElems[t]
			case _: EmptyTuple	=> Nil
		}
}

final case class FieldNames[T](names:Vector[String])
