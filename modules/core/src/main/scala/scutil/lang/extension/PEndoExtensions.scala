package scutil.lang.extension

object PEndoExtensions {
	extension [T](peer:T=>Option[T]) {
		def applyOrOriginal(it:T):T	= peer(it).getOrElse(it)
		def orOriginal:T=>T			= it => peer(it).getOrElse(it)
	}
}
