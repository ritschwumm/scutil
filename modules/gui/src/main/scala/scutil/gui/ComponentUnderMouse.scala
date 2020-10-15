package scutil.gui

import java.lang.ref.WeakReference
import java.awt.Component
import java.awt.MouseInfo
import java.awt.Point
import java.awt.Window
import java.awt.IllegalComponentStateException
import java.applet.Applet

import scala.collection.mutable

import scutil.lang._
import scutil.base.implicits._
import scutil.time._
import scutil.gui.SwingUtil._

object ComponentUnderMouse {
	private type Callback	= Effect[Boolean]

	private final case class Entry(state:Boolean, callbacks:Seq[WeakReference[Callback]]) {
		def referencedCallbacks:Seq[WeakReference[Callback]]	= callbacks filterNot { _.get eq null }
	}
}

/**
get notifications whenever the mouse enters or leaves a Component area.
in contrast to simple mouseEnter/mouseExit events this works when the mouse
moves fast or something is dragged over the component.
*/
final class ComponentUnderMouse(testCycle:MilliDuration, onError:(String,Exception)=>Unit) {
	import ComponentUnderMouse._

	private var entries	= new mutable.WeakHashMap[Component,Entry]

	/** keep a hard reference to the component and either the callback or the resulting disposable or updates will stop */
	def listen(component:Component, callback:Callback):Disposable	= {
		val nowUnderMouse	= underMousePredicate() apply component
		val componentRef	= new WeakReference(callback)
		val newCallbacks	=
			entries get component match {
				case Some(entry)	=> entry.referencedCallbacks :+ componentRef
				case None			=> Vector(componentRef)
			}
		entries	+= (component -> Entry(nowUnderMouse, newCallbacks))
		disposable {
			entries	=
				entries flatMap { case (component, entry) =>
					val newCallbacks	=
							entry.callbacks filterNot { it =>
								val deref	= it.get
								(deref eq null) ||
								deref == callback
							}
					if (newCallbacks.nonEmpty)	List(component -> Entry(entry.state, newCallbacks))
					else						List.empty
				}
		}
	}

	private def update():Unit	= {
		val predicate	= underMousePredicate()
		val updates	=
			for {
				pair	<- entries
				(component, entry)	= pair
				newState	= predicate apply component
				if newState != entry.state
			}
			yield component -> Entry(newState, entry.referencedCallbacks)
		entries	++= updates
		for {
			update			<- updates
			entry			= update._2
			callback		<- entry.callbacks
			callbackHard	= callback.get
			if callbackHard ne null
		} {
			try {
				callbackHard(entry.state)
			}
			catch { case e:Exception	=>
				onError("callback failed", e)
			}
		}
	}

	private def underMousePredicate():Predicate[Component]	=
		mouseLocation.cata(
			Predicates.constFalse,
			mouse	=> underMousePoint(mouse, _)
		)

	/** this is expensive, avoid calls if possible */
	private def mouseLocation:Option[Point]	=
		Option(MouseInfo.getPointerInfo) map { _.getLocation }

	private def underMousePoint(mouse:Point, component:Component):Boolean	= {
		val local	= convertPointFromScreen(mouse, component)
		local.x >= 0	&& local.x	< component.getWidth	&&
		local.y >= 0	&& local.y	< component.getHeight
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
					catch { case e:IllegalComponentStateException	=>
						x	-= cc.getX
						y	-= cc.getY
					}
					return new Point(x, y)
				case _	=>
					x	-= c.getX
					y	-= c.getY
					c	= c.getParent
					if (c eq null) {
						return new Point(x, y)
					}
			}
		}
		nothing
	}

	private val testThread	=
		new Thread {
			override def run():Unit	= {
				while (true) {
					Thread sleep testCycle.millis
					edt {
						try {
							update()
						}
						catch { case e:Exception	=>
							onError("test thread failed", e)
						}
					}
				}
			}
		}
	testThread setName		"ComponentUnderMouse"
	testThread setDaemon	true
	testThread setPriority	Thread.MIN_PRIORITY
	testThread.start()
}
