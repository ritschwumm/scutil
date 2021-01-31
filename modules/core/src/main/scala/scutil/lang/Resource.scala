package scutil.lang

object Resource {
	def apply[T](ev:Resource[T]):Resource[T]	= ev

	def instance[T](disposeFunc:T=>Unit):Resource[T]	=
		new Resource[T] {
			def dispose(it:T):Unit	= disposeFunc(it)
		}

	//------------------------------------------------------------------------------
	//## typeclass instances

	// NOTE Disposer has an instance in its companion already

	implicit def AutoCloseableResource[T<:AutoCloseable]:Resource[T]	= _.close()

	//------------------------------------------------------------------------------

	// TODO should we use Io here?
	//def useIo[R1,T](create:Io[R1])(destroy:R1=>Io[Unit])(consume:R1=>Io[T]):Io[T]	= ???
}

trait Resource[T] {
	def dispose(it:T):Unit

	final def use[U](resource:T)(consume:T=>U):U	=
		Using.unsafeBracket(resource)(dispose)(consume)
}
