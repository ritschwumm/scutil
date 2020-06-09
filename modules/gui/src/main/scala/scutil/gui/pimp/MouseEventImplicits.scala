package scutil.gui.pimp

import java.awt.event._

import scutil.geom._

object MouseEventImplicits extends MouseEventImplicits

trait MouseEventImplicits {
	implicit final class MouseEventExt(peer:MouseEvent) {
		def doublePoint:DoublePoint	=
		DoublePoint(
			peer.getX,
			peer.getY
		)
	}
}
