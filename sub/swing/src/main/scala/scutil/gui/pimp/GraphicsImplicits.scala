package scutil.gui.pimp

import java.awt.Graphics

object GraphicsImplicits extends GraphicsImplicits

trait GraphicsImplicits {
	implicit def toGraphicsExt[T<:Graphics](peer:T)	= new GraphicsExt[T](peer)
}

final class GraphicsExt[T<:Graphics](peer:T) {
	def withClone(effect:T=>Unit) {
		val	g	= peer.create().asInstanceOf[T]
		try { effect(g) }
		finally { g.dispose() }
	}
}
