package scutil.gui

import java.beans._

trait BeanCasterInstances {
	type PropertyChangeCaster = {
		def addPropertyChangeListener(listener:PropertyChangeListener):Unit
		def removePropertyChangeListener(listener:PropertyChangeListener):Unit
	}
	def mkPropertyChangeListener(callback:PropertyChangeEvent=>Unit):PropertyChangeListener = new PropertyChangeListener {
		def propertyChange(ev:PropertyChangeEvent) { callback(ev) }
	}
	implicit def generifyPropertyChangeCaster(caster:PropertyChangeCaster):Caster[PropertyChangeListener,PropertyChangeEvent] = Caster(
			caster.addPropertyChangeListener,
			caster.removePropertyChangeListener,
			mkPropertyChangeListener)
	
	//------------------------------------------------------------------------------

	// NOTE this cannot be made implicit

	type NamedPropertyChangeCaster = {
		def addPropertyChangeListener(name:String, listener:PropertyChangeListener):Unit
		def removePropertyChangeListener(name:String, listener:PropertyChangeListener):Unit
	}
	def generifyNamedPropertyChangeCaster(name:String, caster:NamedPropertyChangeCaster):Caster[PropertyChangeListener,PropertyChangeEvent] = new Caster[PropertyChangeListener,PropertyChangeEvent] {
		def addListener(listener:PropertyChangeListener)	{ caster addPropertyChangeListener		(name, listener) }
		def removeListener(listener:PropertyChangeListener)	{ caster removePropertyChangeListener	(name, listener) }
		def createListener(callback:PropertyChangeEvent=>Unit)	= mkPropertyChangeListener(callback)
	}
}
