package scutil.gui.extension

import java.awt.{ List=>_, _ }

import scutil.geom._
import scutil.gui.geomConversion

object PointImplicits extends PointImplicits

trait PointImplicits {
	implicit final class PointExt(peer:Point) {
		def toDimension:Dimension	=
			new Dimension(
				peer.x,
				peer.y
			)

		def toIntPoint:IntPoint	=
			geomConversion Point_IntPoint peer
	}
}
