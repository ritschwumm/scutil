package scutil.gui.pimp

import java.awt.Graphics

object GraphicsImplicits extends GraphicsImplicits

trait GraphicsImplicits {
	implicit final class GraphicsExt[T<:Graphics](peer:T) {
		def withClone(effect:T=>Unit) {
			@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
			val	g	= peer.create().asInstanceOf[T]
			try { effect(g) }
			finally { g.dispose() }
		}
	}
}
