package scutil.gui.pimp

import java.awt.Rectangle

import scutil.geom._
import scutil.gui.geomConversion

object IntRectImplicits extends IntRectImplicits

trait IntRectImplicits {
	implicit def toIntRectExt(peer:IntRect):IntRectExt	= new IntRectExt(peer)
}
	
final class IntRectExt(peer:IntRect) {
	def toAwtRectangle:Rectangle	=
			geomConversion IntRect_Rectangle peer
}
