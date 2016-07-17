package scutil.io

import java.nio.charset.CharacterCodingException

sealed trait URIComponentProblem
final case class URIComponentInvalid(position:Int)							extends URIComponentProblem
final case class URIComponentException(underlying:CharacterCodingException)	extends URIComponentProblem
