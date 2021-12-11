package scutil.gui.extension

import java.awt.geom._

import scutil.geom._
import scutil.gui.geomConversion

object Dimension2DImplicits {
	implicit final class Dimension2DExt(peer:Dimension2D) {
		def toPoint2D:Point2D	=
			new Point2D.Double(
				peer.getWidth,
				peer.getHeight
			)

		def toDoublePoint:DoublePoint	=
			geomConversion Dimension2D_DoublePoint peer
	}
}
