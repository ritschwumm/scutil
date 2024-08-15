package scutil.lang

import java.util.concurrent.atomic.AtomicReference

object RefUtil {
	/*
	// worked in scala-js before AtomicReference.getAndUpdate was supported
	@tailrec
	def modify[T,U](ref:AtomicReference[T], func:T=>(T,U)):U	= {
		val cur			= ref.get()
		val (next, out)	= func(cur)
		if (ref.compareAndSet(cur, next))	out
		else								modify(ref, func)
	}
	*/

	def modify[T,U](ref:AtomicReference[T], func:T=>(T,U)):U	= {
		@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
		var out:U	= null.asInstanceOf[U]
		ref getAndUpdate { old =>
			val (next, res)	= func(old)
			out	= res
			next
		}
		out
	}
}
