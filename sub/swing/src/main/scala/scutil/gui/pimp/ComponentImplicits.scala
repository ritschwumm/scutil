package scutil.gui.pimp

import java.awt.{ List=>AwtList, _ }
import java.awt.event._
import javax.swing._
import javax.swing.event._

import scutil.lang._
import scutil.geom._
import scutil.gui.geomConversion

object ComponentImplicits extends ComponentImplicits

trait ComponentImplicits {
	implicit def toComponentExt(peer:Component):ComponentExt	= new ComponentExt(peer)
}
	
final class ComponentExt(peer:Component) {
	/** the nearest Window in the ancestor chain, including this component itself */
	def windowSelfOrAncestor:Option[Window]	= 
			windowSelfOrAncestor(peer)
	
	/** the nearest Window in the ancestor chain, excluding this component itself */
	def windowAncestor:Option[Window]	=
			windowSelfOrAncestor(peer.getParent)
	
	private def windowSelfOrAncestor(here:Component):Option[Window]	=
			here match {
				case null		=> None
				case x:Window	=> Some(x)
				case x			=> windowSelfOrAncestor(x.getParent)
			}
	
	/** get the parent Container the scala way */
	def parentOption:Option[Container]	=
			Option(peer.getParent)
		
	/** get all parent Containers starting with the immediate parent and ending with the component root */
	def parentChain:List[Container]	= 
			Lists unfoldRightSimple (
					peer,
					(it:Component) => Option(it.getParent))
		
    /** sets minimum, preferred and maximum size of a {@link Component} */
	def setAllSizes(size:Dimension) {
		peer setMinimumSize		size
		peer setMaximumSize		size
		peer setPreferredSize	size
	}
	
	def outerRectangle:Rectangle =
			new Rectangle(peer.getSize)
			
	def underMousePointer:Boolean	= {
		val	pi	= MouseInfo.getPointerInfo
		if (pi != null)	containsScreenLocation(pi.getLocation )
		else			false
	}
		
	/*
	def underMouseEvent(ev:MouseEvent):Boolean = {
		val	within	= containsScreenLocation(ev.getLocationOnScreen)
		val parent	= SwingUtilities isDescendingFrom (peer, ev.getComponent)
		val exited	= ev.getID() == MouseEvent.MOUSE_EXITED
		within && !(parent && exited)
	}
	*/
	
	def containsScreenLocation(screenLocation:Point):Boolean = {
		val localPosition	= new Point(screenLocation)
		SwingUtilities convertPointFromScreen (localPosition, peer)
		
		val localBounds		= SwingUtilities getLocalBounds peer
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
			geomConversion Rectangle_IntRect peer.getBounds
		
	def setIntBounds(rect:IntRect):Unit	=
			peer setBounds (geomConversion IntRect_Rectangle rect)
}
