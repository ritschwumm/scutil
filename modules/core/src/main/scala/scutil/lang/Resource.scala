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

	@deprecated("use Using", "0.197.0")
	def use[R1,T](create1: =>R1)(consume:(R1)=>T)(implicit R1:Resource[R1]):T	=
		R1.use(create1) { r1 =>
			consume(r1)
		}

	@deprecated("use Using", "0.197.0")
	def use2[R1,R2,T](create1: =>R1, create2: =>R2)(consume:(R1,R2)=>T)(implicit R1:Resource[R1], R2:Resource[R2]):T	=
		R1.use(create1) { r1 =>
			R2.use(create2) { r2 =>
				consume(r1, r2)
			}
		}

	@deprecated("use Using", "0.197.0")
	def use3[R1,R2,R3,T](create1: =>R1, create2: =>R2, create3: =>R3)(consume:(R1,R2,R3)=>T)(implicit R1:Resource[R1], R2:Resource[R2], R3:Resource[R3]):T	=
		R1.use(create1) { r1 =>
			R2.use(create2) { r2 =>
				R3.use(create3) { r3 =>
					consume(r1, r2, r3)
				}
			}
		}

	@deprecated("use Using", "0.197.0")
	def use4[R1,R2,R3,R4,T](create1: =>R1, create2: =>R2, create3: =>R3, create4: =>R4)(consume:(R1,R2,R3,R4)=>T)(implicit R1:Resource[R1], R2:Resource[R2], R3:Resource[R3], R4:Resource[R4]):T	=
		R1.use(create1) { r1 =>
			R2.use(create2) { r2 =>
				R3.use(create3) { r3 =>
					R4.use(create4) { r4 =>
						consume(r1, r2, r3, r4)
					}
				}
			}
		}

	//------------------------------------------------------------------------------

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
