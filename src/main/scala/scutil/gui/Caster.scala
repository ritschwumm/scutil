package scutil.gui

import scutil.Disposable

object Caster {
	def apply[L,E](addFunc:L=>Unit, removeFunc:L=>Unit, createFunc:(E=>Unit)=>L):Caster[L,E] = 
			new CasterImpl[L,E](addFunc, removeFunc, createFunc)
		
}

trait Caster[L,E] extends Connectable[E,Unit] {
	def connect(callback:E=>Unit):Disposable = 
			connect(createListener(callback))
	
	def connect(listener:L):Disposable = {
		addListener(listener)
		new Disposable {
			def dispose() {
				removeListener(listener)
			}
		}
	}
	
	def createListener(callback:E=>Unit):L
	def addListener(listener:L):Unit
	def removeListener(listener:L):Unit
}

private final class CasterImpl[L,E](addFunc:L=>Unit, removeFunc:L=>Unit, createFunc:(E=>Unit)=>L) extends Caster[L,E] {
	def addListener(listener:L):Unit		= addFunc(listener)
	def removeListener(listener:L):Unit		= removeFunc(listener)
	def createListener(callback:E=>Unit):L	= createFunc(callback)
}
