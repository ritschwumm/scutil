package scutil.lang.extension

import java.util.concurrent.Callable

object Function0Extensions {
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
