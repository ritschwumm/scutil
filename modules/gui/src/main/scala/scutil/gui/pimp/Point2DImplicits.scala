package scutil.gui.pimp

import java.awt.geom._

import scutil.geom._
import scutil.gui._

object Point2DImplicits extends Point2DImplicits

trait Point2DImplicits {
	implicit final class Point2DExt(peer:Point2D) {
		def toDimension2D:Dimension2D	=
			new Dimension2D_Double(
				peer.getX,
				peer.getY
			)

		def toDoublePoint:DoublePoint	=
			geomConversion Point2D_DoublePoint peer
	}
}
