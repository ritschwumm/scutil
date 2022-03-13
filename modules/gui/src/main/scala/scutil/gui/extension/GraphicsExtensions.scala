package scutil.gui.extension

import java.awt.Graphics

object GraphicsExtensions {
	implicit final class GraphicsExt[T<:Graphics](peer:T) {
		@deprecated("use Releasable .use syntax", "0.220.0")
		def withClone(effect:T=>Unit):Unit	= {
			@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
			val	g	= peer.create().asInstanceOf[T]
			try {
				effect(g)
			}
			finally {
				g.dispose()
			}
		}
	}
}
