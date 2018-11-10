package scutil.gui.pimp

import java.awt.{ List=>_, _ }

import scutil.geom._
import scutil.gui.geomConversion

object DimensionImplicits extends DimensionImplicits

trait DimensionImplicits {
	implicit final class DimensionExt(peer:Dimension) {
		def toPoint:Point	=
				new Point(
					peer.width,
					peer.height
				)

		def toIntPoint:IntPoint	=
				geomConversion Dimension_IntPoint peer
	}
}
