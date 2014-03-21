package scutil.gui.pimp

import java.awt.Color

import scutil.color._
import scutil.gui.colorConversion

object HSBAImplicits extends HSBAImplicits

trait HSBAImplicits {
	implicit def toHSBAExt(peer:HSBA):HSBAExt	= new HSBAExt(peer)
}
	
final class HSBAExt(peer:HSBA) {
	def toColor:Color	=
			colorConversion	HSBA_Color(peer)
}
