package scutil.lang.tc

object Resource {
	def apply[T:Resource]:Resource[T]	= implicitly[Resource[T]]
	
	def by[T](disposeFunc:T=>Unit):Resource[T]	=
			new Resource[T] {
				def dispose(it:T):Unit	= disposeFunc(it)
			}
}

trait Resource[T] {
	def dispose(it:T):Unit
	
	final def use[U](resource:T)(consume:T=>U):U	= {
		var primary:Throwable	= null
		try {
			consume(resource)
		}
		catch { case e:Throwable	=>
			primary	= e
			throw e
		}
		finally {
			if (primary ne null) {
				try {
					dispose(resource)
				}
				catch { case e:Throwable	=>
					primary addSuppressed e
				}
			}
			else {
				dispose(resource)
			}
		}
	}
}
