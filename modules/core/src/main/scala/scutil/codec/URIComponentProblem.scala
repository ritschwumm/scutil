package scutil.codec

import java.nio.charset.CharacterCodingException

enum URIComponentProblem {
	case Invalid(position:Int)
	case Exception(underlying:CharacterCodingException)
}
