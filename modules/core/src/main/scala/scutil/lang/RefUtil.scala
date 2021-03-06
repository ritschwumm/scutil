package scutil.lang

import scala.annotation.tailrec

import java.util.concurrent.atomic.AtomicReference

object RefUtil {
	/** works in scala-js where AtomicReference.getAndUpdate is not supported */
	@tailrec
	def modify[T,U](ref:AtomicReference[T], func:T=>(T,U)):U	= {
		val cur			= ref.get()
		val (next, out)	= func(cur)
		if (ref.compareAndSet(cur, next))	out
		else								modify(ref, func)
	}
}
