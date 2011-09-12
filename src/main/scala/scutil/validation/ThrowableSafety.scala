package scutil.validation

import scala.util.control.Exception._

import ValidationInstances._

/** extends scala.util.control.Exception handling so it can be used with Validated */
object ThrowableSafety {
	// TODO is harcoding to NEL here useful in any way?
	type ThrowableSafe[T]	= Validated[NonEmptyList[Throwable],T]
	
	implicit def SafeCatch[T](delegate:Catch[T]) = new {
		def validated[U>:T](body: =>U):ThrowableSafe[U] = toValidated(Valid(body))
		
		def toValidated:Catch[ThrowableSafe[T]]	= delegate withApply {it =>
			Validated invalid NonEmptyList(it)
		}
	}
}
