package scutil.gui.pimp

import java.awt.Color

import scutil.color._
import scutil.gui.colorConversion

object HSBImplicits extends HSBImplicits

trait HSBImplicits {
	implicit final class HSBExt(peer:HSB) {
		def toColor:Color	=
				colorConversion	HSB_Color(peer)
	}
}
