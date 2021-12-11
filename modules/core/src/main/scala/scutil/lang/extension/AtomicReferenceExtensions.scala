package scutil.lang.extension

import java.util.concurrent.atomic.AtomicReference

import scutil.lang.*

object AtomicReferenceExtensions {
	/** works in scala-js where AtomicReference.getAndUpdate is not supported */
	implicit final class AtomicReferenceExt[T](peer:AtomicReference[T]) {
		def modify[U](func:T=>(T,U)):U	= RefUtil.modify(peer, func)
	}
}
