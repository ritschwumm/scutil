package scutil.lang

import scutil.lang.tc._

object Disposer {
	def delay(todo: =>Unit):Disposer	= Disposer(() => todo)

	/** forms a monoids with and */
	val empty:Disposer	= Disposer(() => ())

	def combineAll(subs:Iterable[Disposer]):Disposer	=
		subs.foldLeft(empty)(_ combine _)

	def combineOf(subs:Disposer*):Disposer	=
		combineAll(subs.toVector)

	// TODO this shows how wrong having Disposer is
	def fromIo(io:Io[Unit]):Disposer	=
		Disposer(io.unsafeRun _)

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit val DisposerMonoid:Monoid[Disposer]			= Monoid.instance(empty, _ combine _)
	implicit def DisposerResource[T<:Disposer]:Resource[T]	= _.dispose()
}

final case class Disposer(dispose:()=>Unit) {
	/**
	 * forms a monoid with empty
	 * in case of exceptions, the first occuring one is thrown, if a second occurs it's addSuppressed to the first
	 */
	final def combine(that:Disposer):Disposer	=
			 if (this == Disposer.empty)	that
		else if (that == Disposer.empty)	this
		else {
			Disposer delay {
				var thisError:Throwable	= null;	try { this.dispose() } catch { case t:Throwable	=> thisError	= t }
				var thatError:Throwable	= null;	try { that.dispose() } catch { case t:Throwable	=> thatError	= t }
					 if ((thisError ne null) && (thatError ne null))	{ thisError.addSuppressed(thatError);	throw thisError }
				else if (thisError ne null) 																	throw thisError
				else if (thatError ne null) 																	throw thatError
			}
		}

	final def toIo:Io[Unit]	=
		Io delay { dispose() }
}
