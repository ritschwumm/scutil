package scutil.gui.extension

import java.awt.{ List as _, * }
import java.awt.geom.*

import scutil.gui.*

object DimensionConversionExtensions {
	extension (peer:Dimension2D) {
		def toPoint2D:Point2D	=
			new Point2D.Double(
				peer.getWidth,
				peer.getHeight
			)
	}

	extension (peer:Dimension) {
		def toPoint:Point	=
			new Point(
				peer.width,
				peer.height
			)
	}

	extension (peer:Point2D) {
		def toDimension2D:Dimension2D	=
			new Dimension2D_Double(
				peer.getX,
				peer.getY
			)
	}

	extension (peer:Point) {
		def toDimension:Dimension	=
			new Dimension(
				peer.x,
				peer.y
			)
	}
}
