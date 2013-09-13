package scutil.pimp

import java.util.concurrent.Callable

import scutil.lang.Disposable

object Function0Implicits extends Function0Implicits

trait Function0Implicits {
	implicit def toFunction0Ext[T](delegate:Function0[T]) = new Function0Ext[T](delegate)
}

final class Function0Ext[T](delegate:Function0[T]) {
	def asRunnable:Runnable = 
			new Runnable {
				def run() {
					delegate()
				}
			}
	
	def asCallable:Callable[T]	=
			new Callable[T] {
				def call() = delegate()
			}
}
