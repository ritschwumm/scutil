package scutil.number.extension

import scutil.number.BigRational

object StringContextImplicits {
	extension (inline peer:StringContext) {
		inline def br(inline parts:Any*):BigRational	= ${ BigRationalMacros.br('peer) }
	}
}
