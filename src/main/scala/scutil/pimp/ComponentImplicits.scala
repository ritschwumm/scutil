package scutil.pimp

import java.awt.{ List=>AwtList, _ }
import java.awt.event._
import javax.swing._
import javax.swing.event._

import scutil.Lists
import scutil.geom._

import RectangleImplicits._

object ComponentImplicits extends ComponentImplicits

trait ComponentImplicits {
	implicit def toComponentExt(delegate:Component):ComponentExt	= new ComponentExt(delegate)
}
	
final class ComponentExt(delegate:Component) {
	/** the nearest Window in the ancestor chain, including this component itself */
	def windowSelfOrAncestor:Option[Window]	= 
			windowSelfOrAncestor(delegate)
	
	/** the nearest Window in the ancestor chain, excluding this component itself */
	def windowAncestor:Option[Window]	=
			windowSelfOrAncestor(delegate.getParent)
	
	private def windowSelfOrAncestor(here:Component):Option[Window]	=
			here match {
				case null		=> None
				case x:Window	=> Some(x)
				case x			=> windowSelfOrAncestor(x.getParent)
			}
	
	/** get the parent Container the scala way */
	def parentOption:Option[Container]	=
			Option(delegate.getParent)
		
	/** get all parent Containers starting with the immediate parent and ending with the component root */
	def parentChain:List[Container]	= 
			Lists unfoldRightSimple (
					delegate,
					(it:Component) => Option(it.getParent))
		
    /** sets minimum, preferred and maximum size of a {@link Component} */
	def setAllSizes(size:Dimension) {
		delegate setMinimumSize	size
		delegate setMaximumSize	size
		delegate setPreferredSize	size
	}
	
	def outerRectangle:Rectangle =
			new Rectangle(delegate.getSize)
			
	def underMousePointer:Boolean	= {
		val	pi	= MouseInfo.getPointerInfo
		if (pi != null)	containsScreenLocation(pi.getLocation )
		else			false
	}
		
	/*
	def underMouseEvent(ev:MouseEvent):Boolean = {
		val	within	= containsScreenLocation(ev.getLocationOnScreen)
		val parent	= SwingUtilities isDescendingFrom (delegate, ev.getComponent)
		val exited	= ev.getID() == MouseEvent.MOUSE_EXITED
		within && !(parent && exited)
	}
	*/
	
	def containsScreenLocation(screenLocation:Point):Boolean = {
		val localPosition	= new Point(screenLocation)
		SwingUtilities convertPointFromScreen (localPosition, delegate)
		
		val localBounds		= SwingUtilities getLocalBounds delegate
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
	
	def getIntBounds:IntRect	=
			IntRect fromAwtRectangle delegate.getBounds
		
	def setIntBounds(rect:IntRect):Unit	=
			delegate setBounds rect.toAwtRectangle
}
