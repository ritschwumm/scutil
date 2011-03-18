package scutil.ext

import java.awt.{ List=>AwtList, _ }
import java.awt.event._
import javax.swing._
import javax.swing.event._

import RectangleImplicits._

object ComponentImplicits extends ComponentImplicits

trait ComponentImplicits {
	implicit def toComponentExt[T <: Component](delegate:T):ComponentExt[T] = new ComponentExt[T](delegate)
}
	
final class ComponentExt[T <: Component](delegate:T) {
	/** sets minimum, preferred and maximum size of a {@link Component} */
	def setAllSizes(size:Dimension) {
		delegate setMinimumSize	size
		delegate setMaximumSize	size
		delegate setPreferredSize	size
	}
	
	def outerRectangle:Rectangle =
			new Rectangle(delegate.getSize)
			
	def mouseHovers(ev:MouseEvent):Boolean = {
		val	within	= containsLocationOnScreen(ev.getLocationOnScreen)
		val parent	= SwingUtilities isDescendingFrom (delegate, ev.getComponent)
		val exited	= ev.getID() == MouseEvent.MOUSE_EXITED
		within && !(parent &&  exited)
	}
	
	def containsLocationOnScreen(screenLocation:Point):Boolean = {
		val localBounds		= SwingUtilities getLocalBounds delegate
		val localPosition	= new Point(screenLocation)
		SwingUtilities convertPointFromScreen (localPosition, delegate)
		localBounds contains localPosition
	}
	
	/*
	def componentsUnder(parent:Component, pos:Point):List[Component] =
			if (parent.isVisible && (parent contains (pos.x,pos.y)))
				parent :: 
				(parent match {
					case cont:Container	=>
						cont.getComponents 
						.filter { _ != null } 
						.toList
						.flatMap { it =>
							val	loc	= it.getLocation
							componentsUnder(it, new Point(
									pos.x - loc.x, 
									pos.y - loc.y))
						}
					case comp:Component	=>
						Nil
				})
			else
				Nil
	*/
}
