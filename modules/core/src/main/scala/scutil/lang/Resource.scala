package scutil.lang

object Resource {
	def apply[T](ev:Resource[T]):Resource[T]	= ev

	def instance[T](disposeFunc:T=>Unit):Resource[T]	=
		new Resource[T] {
			def dispose(it:T):Unit	= disposeFunc(it)
		}

	//------------------------------------------------------------------------------
	//## typeclass instances

	// NOTE Disposable has an instance in its companion already

	implicit def AutoCloseableResource[T<:AutoCloseable]:Resource[T]	= _.close()

	//------------------------------------------------------------------------------

	// TODO should we use Io here?
	//def useIo[R1,T](create:Io[R1])(destroy:R1=>Io[Unit])(consume:R1=>Io[T]):Io[T]	= ???

	def bracket[T,U](resource:T)(dispose:T=>Unit)(consume:T=>U):U	= {
		var primary:Throwable	= null
		try {
			consume(resource)
		}
		catch { case e:Throwable	=>
			primary	= e
			throw e
		}
		finally {
			try {
				dispose(resource)
			}
			catch { case e:Throwable	=>
				if (primary ne null)	primary addSuppressed e
				else					throw e
			}
		}
	}
}

trait Resource[T] {
	def dispose(it:T):Unit

	final def use[U](resource:T)(consume:T=>U):U	=
		Resource.bracket(resource)(dispose)(consume)
}
