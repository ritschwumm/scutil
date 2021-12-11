package scutil.gui.extension

import java.awt.event._

import scutil.geom._

object MouseEventImplicits {
	implicit final class MouseEventExt(peer:MouseEvent) {
		def doublePoint:DoublePoint	=
			DoublePoint(
				peer.getX,
				peer.getY
			)
	}
}
