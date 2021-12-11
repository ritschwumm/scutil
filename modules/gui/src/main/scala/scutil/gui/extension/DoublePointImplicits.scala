package scutil.gui.extension

import java.awt.geom._

import scutil.geom._
import scutil.gui.geomConversion

object DoublePointImplicits {
	implicit final class DoublePointExt(peer:DoublePoint) {
		def toAwtPoint2D:Point2D			= geomConversion	DoublePoint_Point2D		peer
		def toAwtDimension2D:Dimension2D	= geomConversion	DoublePoint_Dimension2D	peer
	}
}
