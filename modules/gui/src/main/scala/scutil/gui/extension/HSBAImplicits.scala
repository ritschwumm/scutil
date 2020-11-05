package scutil.gui.extension

import java.awt.Color

import scutil.color._
import scutil.gui.colorConversion

object HSBAImplicits extends HSBAImplicits

trait HSBAImplicits {
	implicit final class HSBAExt(peer:HSBA) {
		def toColor:Color	= colorConversion	HSBA_Color(peer)
	}
}
