package scutil.lang

import scutil.lang.tc.*

object IoDisposer {
	def delay(block: =>Unit):IoDisposer	=
		IoDisposer(Io delay block)

	/** forms a monoids with combine */
	val empty:IoDisposer	= IoDisposer(Io.unit)

	def combineAll(subs:Iterable[IoDisposer]):IoDisposer	=
		subs.foldLeft(empty)(_ combine _)

	def combineOf(subs:IoDisposer*):IoDisposer	=
		combineAll(subs.toVector)

	//------------------------------------------------------------------------------
	//## typeclass instances

	given IoDisposerMonoid:Monoid[IoDisposer]	= Monoid.instance(empty, _ combine _)
}

final case class IoDisposer(toIo:Io[Unit]) {
	def unsafeRun():Unit	= toIo.unsafeRun()

	/**
	 * forms a monoid with empty
	 * in case of exceptions, the first occuring one is thrown, if a second occurs it's addSuppressed to the first
	 */
	final def combine(that:IoDisposer):IoDisposer	=
		IoDisposer(this.toIo.guarantee(that.toIo))
}
