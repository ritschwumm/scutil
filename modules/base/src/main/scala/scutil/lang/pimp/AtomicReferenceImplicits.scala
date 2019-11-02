package scutil.lang.pimp

import java.util.concurrent.atomic.AtomicReference

import scutil.lang._

object AtomicReferenceImplicits extends AtomicReferenceImplicits

trait AtomicReferenceImplicits {
	implicit final class AtomicReferenceExt[T](peer:AtomicReference[T]) {
		def modify[U](func:T=>(T,U)):U	= RefUtil.modify(peer, func)
	}
}
