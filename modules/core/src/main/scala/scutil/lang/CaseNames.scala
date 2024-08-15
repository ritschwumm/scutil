package scutil.lang

import scala.deriving.*
import scala.compiletime.*

object CaseNames {
	inline given of[T](using m:Mirror.SumOf[T]):CaseNames[T]	=
		CaseNames(
			constValueTuple[m.MirroredElemLabels]
			.productIterator
			.map(_.toString)
			.toVector
		)
}

final case class CaseNames[T](names:Vector[String])
