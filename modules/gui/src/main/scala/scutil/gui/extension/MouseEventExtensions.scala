package scutil.gui.extension

import java.awt.event.*

import scutil.geom.*

object MouseEventExtensions {
	extension (peer:MouseEvent) {
		def doublePoint:DoublePoint	=
			DoublePoint(
				peer.getX,
				peer.getY
			)

		def intPoint:IntPoint	=
			IntPoint(
				peer.getX,
				peer.getY
			)
	}
}
