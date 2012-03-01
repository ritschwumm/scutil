package scutil.ext

import java.awt.Graphics

object GraphicsImplicits extends GraphicsImplicits

trait GraphicsImplicits {
	implicit def toGraphicsExt[T<:Graphics](delegate:T)	= new GraphicsExt[T](delegate)
}

final class GraphicsExt[T<:Graphics](delegate:T) {
	def withClone(effect:T=>Unit) {
		val	g	= delegate.create().asInstanceOf[T]
		try { effect(g) }
		finally { g.dispose() }
	}
}
