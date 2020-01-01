package scutil.gui.pimp

import java.awt._

import scutil.geom._
import scutil.gui.geomConversion

object IntRectImplicits extends IntRectImplicits

trait IntRectImplicits {
	implicit final class IntRectExt(peer:IntRect) {
		def toAwtRectangle:Rectangle	= geomConversion IntRect_Rectangle peer
	}
}
