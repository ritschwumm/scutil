package scutil.lang.extension

object AnyRefExtensions {
	extension [T <: AnyRef](peer:T) {
		/** Some if not null, None if null */
		def optionNotNull:Option[T]	=
			if (peer != null)	Some(peer)
			else				None

		/** replace null with another value */
		def replaceNull[U>:T](replacement: =>U):U	=
			if (peer != null)	peer
			else				replacement

		/** fail with an Exception if null */
		def nullError(s: =>String):T	=
			if (peer != null)	peer
			else				sys error s
	}
}
