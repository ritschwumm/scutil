package scutil.lang.extension

import java.util.concurrent.atomic.AtomicReference

import scutil.lang.*

object AtomicReferenceExtensions {
	/** works in scala-js where AtomicReference.getAndUpdate is not supported */
	extension [T](peer:AtomicReference[T]) {
		def modify[U](func:T=>(T, U)):U	= RefUtil.modify(peer, func)

		/** change state and return something */
		def modifyState[U](state:State[T, U]):U	=
			modify(state.run)

		/*
		NOTE no need to add these:
		Synchronized.get exists as AtomicReference.getAndSet
		Synchronized.update exists as AtomicReference.getAndUpdate
		*/
	}
}
