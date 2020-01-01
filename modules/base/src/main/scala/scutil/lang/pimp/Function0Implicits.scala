package scutil.lang.pimp

import java.util.concurrent.Callable

object Function0Implicits extends Function0Implicits

trait Function0Implicits {
	implicit final class Function0Ext[T](peer:Function0[T]) {
		def toRunnable:Runnable =
			new Runnable {
				def run():Unit	= {
					peer()
				}
			}

		def toCallable:Callable[T]	=
			new Callable[T] {
				def call() = peer()
			}
	}
}
