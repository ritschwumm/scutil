package scutil.gui.extension

import java.awt.{ List as _, * }

import scutil.geom.*
import scutil.gui.geomConversion

object WindowExtensions {
	extension (peer:Window) {
		def restrictToScreen():Unit	= {
			val frame	= geomConversion.Rectangle_IntRect(peer.getBounds)
			val screen	= geomConversion.Rectangle_IntRect(peer.getGraphicsConfiguration.getBounds)

			def restrict(frame:IntSpan, screen:IntSpan):IntSpan	=
				if		(frame.size		> screen.size)	screen
				else if (frame.start	< screen.start)	IntSpan.startSize(screen.start,				frame.size)
				else if (frame.end		> screen.end)	IntSpan.startSize(screen.end - frame.size,	frame.size)
				else									frame

			val bounds	=
				IntRect.horizontalWithVertical(
					horizontal	= restrict(frame.horizontal,	screen.horizontal),
					vertical	= restrict(frame.vertical,		screen.vertical)
				)

			peer.setBounds(geomConversion.IntRect_Rectangle(bounds))
		}
	}
}
