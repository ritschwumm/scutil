package scutil.gui.extension

import java.awt.geom._

import scutil.geom._
import scutil.gui.geomConversion

object DoubleRectImplicits {
	implicit final class DoubleRectExt(peer:DoubleRect) {
		def toAwtRectangle2D:Rectangle2D	= geomConversion DoubleRect_Rectangle2D peer
	}
}
