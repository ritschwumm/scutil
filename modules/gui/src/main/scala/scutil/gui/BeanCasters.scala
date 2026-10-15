package scutil.gui

import scala.reflect.Selectable.reflectiveSelectable

import java.beans.*

import scutil.lang.*

object BeanCasters {
	type PropertyChangeMethods = {
		def addPropertyChangeListener(listener:PropertyChangeListener):Unit
		def removePropertyChangeListener(listener:PropertyChangeListener):Unit
	}

	extension (peer:PropertyChangeMethods) {
		def propertyChangeCaster:Caster[PropertyChangeListener]	= new Caster(peer.addPropertyChangeListener, peer.removePropertyChangeListener)

		// def onPropertyChangeEvent(callback:PropertyChangeEvent=>Unit):Disposer	= propertyChangeCaster.listen(callback(_))

		def onPropertyChange(callback:PropertyChangeEvent=>Unit):Disposer	= propertyChangeCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	/*
	type NamedPropertyChangeCaster = {
		def addPropertyChangeListener(name:String, listener:PropertyChangeListener):Unit
		def removePropertyChangeListener(name:String, listener:PropertyChangeListener):Unit
	}
	// NOTE this cannot be made implicit
	def generifyNamedPropertyChangeCaster(name:String, caster:NamedPropertyChangeCaster):Caster[PropertyChangeListener,PropertyChangeEvent] = new Caster[PropertyChangeListener,PropertyChangeEvent] {
		def addListener(listener:PropertyChangeListener)	{ caster addPropertyChangeListener		(name, listener) }
		def removeListener(listener:PropertyChangeListener)	{ caster removePropertyChangeListener	(name, listener) }
		def createListener(callback:PropertyChangeEvent=>Unit)	= mkPropertyChangeListener(callback)
	}
	*/
}
