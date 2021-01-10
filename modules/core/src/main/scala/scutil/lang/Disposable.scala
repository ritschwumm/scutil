package scutil.lang

import scutil.lang.tc._

object Disposable {
	def delay(todo: =>Unit):Disposable	= Disposable(() => todo)

	/** forms a monoids with and */
	val empty:Disposable	= Disposable(() => ())

	// TODO use Monoid.combineAll
	@deprecated("use combineAll", "0.197.0")
	def all(subs:Seq[Disposable]):Disposable	= combineAll(subs)

	def combineAll(subs:Seq[Disposable]):Disposable	=
		delay {
			subs foreach {
				_.dispose()
			}
		}

	// TODO use Monoid.combineOf?
	@deprecated("use combineOf", "0.197.0")
	def allVar(subs:Disposable*):Disposable		= combineOf(subs:_*)

	def combineOf(subs:Disposable*):Disposable	= combineAll(subs.toVector)

	// TODO this shows how wrong having Disposable is
	def fromIo(io:Io[Unit]):Disposable	=
		Disposable(io.unsafeRun _)

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit val DisposableMonoid:Monoid[Disposable]			= Monoid.instance(Disposable.empty, _ combine _)
	implicit def DisposableResource[T<:Disposable]:Resource[T]	= _.dispose()
}

final case class Disposable(dispose:()=>Unit) {
	@deprecated("use combine", "0.197.0")
	final def and(that:Disposable):Disposable	=
		this combine that

	/**
	 * forms a monoid with empty
	 * in case of exceptions, the first occuring one is thrown, if a second occurs it's addSuppressed to the first
	 */
	final def combine(that:Disposable):Disposable	=
			 if (this == Disposable.empty)	that
		else if (that == Disposable.empty)	this
		else {
			Disposable delay {
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
