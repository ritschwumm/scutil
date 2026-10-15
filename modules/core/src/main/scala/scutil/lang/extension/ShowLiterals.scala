package scutil.lang.extension

import scala.Conversion
import scala.Conversion.into

import scutil.lang.tc.Show

object ShowLiterals {
	extension (peer:StringContext) {
		/** provide a string interpolator "show" that requires a Show instance for all arguments */
		def show(args:into[Shown]*):String	= peer.s(args.map(Shown.unwrap)*)
	}

	//-----------------------------------------------------------------------------

	/** helper to allow using the into[] syntax for the show interpolator */
	object Shown {
		given [T:Show as T] => Conversion[T, Shown]	= it => T.show(it)

		def unwrap(self:Shown):String	= self
	}

	opaque type Shown	= String
}
