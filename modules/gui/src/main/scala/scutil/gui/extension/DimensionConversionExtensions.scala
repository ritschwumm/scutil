package scutil.gui.extension

import java.awt.{ List as _, * }
import java.awt.geom.*

import scutil.gui.*

object DimensionConversionExtensions {
	implicit final class DimensionConversionDimension2DExt(peer:Dimension2D) {
		def toPoint2D:Point2D	=
			new Point2D.Double(
				peer.getWidth,
				peer.getHeight
			)
	}

	implicit final class DimensionConversionDimensionExt(peer:Dimension) {
		def toPoint:Point	=
			new Point(
				peer.width,
				peer.height
			)
	}

	implicit final class DimensionConversionPoint2DExt(peer:Point2D) {
		def toDimension2D:Dimension2D	=
			new Dimension2D_Double(
				peer.getX,
				peer.getY
			)
	}

	implicit final class DimensionConversionPointExt(peer:Point) {
		def toDimension:Dimension	=
			new Dimension(
				peer.x,
				peer.y
			)
	}
}
