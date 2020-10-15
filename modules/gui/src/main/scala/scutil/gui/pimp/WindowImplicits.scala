package scutil.gui.pimp

import java.awt.{ List=>_, _ }

import scutil.geom._
import scutil.gui.geomConversion

object WindowImplicits extends WindowImplicits

trait WindowImplicits {
	implicit final class WindowExt(peer:Window) {
		def restrictToScreen():Unit	= {
			val frame	= geomConversion Rectangle_IntRect peer.getBounds
			val screen	= geomConversion Rectangle_IntRect peer.getGraphicsConfiguration.getBounds

			def restrict(frame:IntSpan, screen:IntSpan):IntSpan	=
					 if (frame.size		> screen.size)	screen
				else if (frame.start	< screen.start)	IntSpan.startSize(screen.start,				frame.size)
				else if (frame.end		> screen.end)	IntSpan.startSize(screen.end - frame.size,	frame.size)
				else									frame

			val bounds	=
				IntRect.horizontalWithVertical(
					horizontal	= restrict(frame.horizontal, screen.horizontal),
					vertical	= restrict(frame.vertical,	 screen.vertical)
				)

			peer setBounds (geomConversion IntRect_Rectangle bounds)
		}
	}
}
