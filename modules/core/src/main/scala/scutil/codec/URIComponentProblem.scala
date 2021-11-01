package scutil.codec

import java.nio.charset.CharacterCodingException

object URIComponentProblem {
	final case class Invalid(position:Int)							extends URIComponentProblem
	final case class Exception(underlying:CharacterCodingException)	extends URIComponentProblem
}

sealed trait URIComponentProblem
