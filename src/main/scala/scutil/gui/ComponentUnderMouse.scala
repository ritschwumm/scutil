package scutil.gui

import java.awt.{ List=>AwtList, _ }
import java.awt.event._
import java.awt.dnd._
import javax.swing._
import java.applet.Applet
import java.lang.ref.WeakReference

import scala.collection.mutable

import scutil.lang._
import scutil.log._

/** 
get notifications whenever the mouse moves over a Component or leaves it.
in contrast to simple mouseEnter/mouseExit events this works when the mouse
moves fast or something is dragged over the component.
*/
object ComponentUnderMouse extends Logging {
	private val TEST_CYCLE	= 100	// millis
	
	private type Callback	= Effect[Boolean]
 	
	private var entries	= new mutable.WeakHashMap[Component,Entry]
	
	private case class Entry(state:Boolean, callbacks:Seq[WeakReference[Callback]])
	
	def listen(component:Component, callback:Effect[Boolean]):Disposable	= {
		val nowUnderMouse	= underMousePointer(component)
		entries get component match {
			case Some(entry)	=> 
				val newCallbacks	= 
						(entry.callbacks filterNot { it:WeakReference[Callback] => it.get == null })	:+
						new WeakReference(callback)
				entries	+= (component -> Entry(nowUnderMouse, newCallbacks))
			case None			=>
				entries	+= (component -> Entry(nowUnderMouse, Vector(new WeakReference(callback))))
				component	addComponentListener	ComponentHandler
				component	addMouseListener		MouseHandler
				// TODO breaks on change
				val dropTarget	= component.getDropTarget
				if (dropTarget != null) {
					dropTarget addDropTargetListener DropHandler
				}
		}
		Disposable {
			entries	= entries flatMap { case (component,entry) =>
				val newCallbacks	= entry.callbacks filterNot { it:WeakReference[Callback] => 
					it.get == null || it.get == callback 
				}
				if (newCallbacks.nonEmpty)	Some((component, Entry(entry.state, newCallbacks)))
				else						None
			}
			component removeComponentListener	ComponentHandler
			component removeMouseListener		MouseHandler
			// TODO breaks on change
			val dropTarget	= component.getDropTarget
			if (dropTarget != null) {
				dropTarget removeDropTargetListener DropHandler
			}
		}
	}
	
	private def update() {
		val updates	= entries flatMap { case (component,entry)	=>
			val newState	= underMousePointer(component)
			if (newState != entry.state)	Some((component, entry copy (state = newState)))
			else							None
		}
		entries	++= updates
		for {
			update			<- updates
			entry			= update._2
			callback		<- entry.callbacks
			callbackHard	= callback.get
			if callbackHard != null
		} {
			callbackHard(entry.state)
		}
	}
	
	// import scutil.Implicits._
	// private def underMousePointer(delegate:Component):Boolean = delegate.underMousePointer
	
	private def underMousePointer(delegate:Component):Boolean = {
		val	pi	= MouseInfo.getPointerInfo
		if (pi != null) {
			val localPosition	= convertPointFromScreen (pi.getLocation, delegate)
			localPosition.x >= 0				&&
			localPosition.y >= 0				&&
			localPosition.x	< delegate.getWidth	&&
			localPosition.y	< delegate.getHeight
		}
		else {
			false
		}
	}
	
   private def convertPointFromScreen(point:Point, component:Component):Point	= {
    	var x:Int		= point.x
    	var y:Int		= point.y
    	var c:Component	= component
    	
    	while (true) {
			c match {
				case cc @ (_:Applet | _:Window) =>
					try {
						val	pp	= cc.getLocationOnScreen
						x	-= pp.x
						y	-= pp.y
					}
					catch {
						case e:IllegalComponentStateException	=>
							x	-= cc.getX
							y	-= cc.getY
					}
					return new Point(x,y)
				case _	=>
					x	-= c.getX
					y	-= c.getY
					c	= c.getParent
					if (c == null) {
						return new Point(x,y)
					}
			}
		}
		neverComesHere
	}
	
	private object ComponentHandler extends ComponentAdapter {
		override def componentHidden(ev:ComponentEvent)		{ update()	}
		override def componentMoved(ev:ComponentEvent)		{ update()	}
		override def componentResized(ev:ComponentEvent)	{ update()	}
		override def componentShown(ev:ComponentEvent)		{ update()	}
    }
    
    private object MouseHandler extends MouseAdapter {
		override def mouseEntered(ev:MouseEvent)	{ update() }
		override def mouseExited(ev:MouseEvent)		{ update() }
	}
	
	private object DropHandler extends DropTargetAdapter {
		override def dragEnter(ev:DropTargetDragEvent)	{ update() }
		override def dragExit(ev:DropTargetEvent)		{ update() }
		override def drop(ev:DropTargetDropEvent)		{}
	}
	
	private val testThread	= new Thread {
		override def run() {
			while (true) {
				Thread sleep TEST_CYCLE
				SwingUtilities invokeLater new Runnable {
					def run() {
						try {
							update()
						}
						catch {
							case e	=> ERROR(e)
						}
					}
				}
			}
		}
	} 
	testThread setName		"ComponentUnderMouse"
	testThread setDaemon	true
	testThread setPriority	Thread.MIN_PRIORITY
	testThread start
}
