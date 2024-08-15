package scutil.gui.extension

import java.awt.{ List as _, * }
import java.awt.geom.*

import scutil.geom.*
import scutil.gui.geomConversion

object GeometryConversionExtensions {
	//------------------------------------------------------------------------------
	//## from awt

	extension (peer:Dimension2D) {
		def toDoublePoint:DoublePoint	= geomConversion.Dimension2D_DoublePoint(peer)
	}

	extension (peer:Dimension) {
		def toIntPoint:IntPoint			= geomConversion.Dimension_IntPoint(peer)
	}

	extension (peer:Point2D) {
		def toDoublePoint:DoublePoint	= geomConversion.Point2D_DoublePoint(peer)
	}

	extension (peer:Point) {
		def toIntPoint:IntPoint			= geomConversion.Point_IntPoint(peer)
	}

	extension (peer:Rectangle2D) {
		def toDoubleRect:DoubleRect		= geomConversion.Rectangle2D_DoubleRect(peer)
	}

	extension (peer:Rectangle) {
		def toIntRect:IntRect			= geomConversion.Rectangle_IntRect(peer)
	}

	//------------------------------------------------------------------------------
	//## to awt

	extension (peer:DoublePoint) {
		def toAwtPoint2D:Point2D			= geomConversion.DoublePoint_Point2D(peer)
		def toAwtDimension2D:Dimension2D	= geomConversion.DoublePoint_Dimension2D(peer)
	}

	extension (peer:DoubleRect) {
		def toAwtRectangle2D:Rectangle2D	= geomConversion.DoubleRect_Rectangle2D(peer)
	}

	extension (peer:IntPoint) {
		def toAwtPoint:Point			= geomConversion.IntPoint_Point(peer)
		def toAwtDimension:Dimension	= geomConversion.IntPoint_Dimension(peer)
	}

	extension (peer:IntRect) {
		def toAwtRectangle:Rectangle	= geomConversion.IntRect_Rectangle(peer)
	}
}

/*
extension (peer:Rectangle2D) {
	// NOTE if we had this, it wouldn't belong here
	def inset(insets:Insets):Rectangle2D	=
		new Rectangle2D(
			peer.x + insets.left,
			peer.y + insets.top,
			peer.width	- insets.left	- insets.right,
			peer.height	- insets.top	- insets.bottom
		)
}
extension (peer:Rectangle) {
	// NOTE if we had this, it wouldn't belong here
	def inset(insets:Insets):Rectangle	=
		new Rectangle(
			peer.x + insets.left,
			peer.y + insets.top,
			peer.width	- insets.left	- insets.right,
			peer.height	- insets.top	- insets.bottom
		)
}
*/
