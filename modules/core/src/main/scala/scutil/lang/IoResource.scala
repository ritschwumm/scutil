package scutil.lang

import  scala.util.Using.Releasable

import scutil.lang.tc._

object IoResource {
	def pure[T](value:T):IoResource[T]	=
		IoResource(Io.pure(value -> Io.unit))

	def delay[T](value: =>T):IoResource[T]	=
		IoResource(Io.delay(value -> Io.unit))

	def lift[T](value:Io[T]):IoResource[T]	=
		IoResource(value.map(_ -> Io.unit))

	def disposing[T](create:Io[T])(dispose:T=>Io[Unit]):IoResource[T]	=
		IoResource(create map { t => t -> dispose(t) })

	def releasable[T](create:Io[T])(implicit R:Releasable[T]):IoResource[T]	=
		IoResource(create map { t => t -> Io.delay{ R.release(t) } })

	def lifecycle(before:Io[Unit], after:Io[Unit]):IoResource[Unit]	=
		IoResource(before.map(_ -> after))

	def afterwards(dispose:Io[Unit]):IoResource[Unit]	=
		IoResource(Io.pure(() -> dispose))

	//------------------------------------------------------------------------------

	object unsafe {
		def of[T](value: =>T):IoResource[T]	=
			IoResource.delay(value)

		def disposing[T](create: =>T)(dispose:T=>Unit):IoResource[T]	=
			IoResource.disposing(Io delay create)(t => Io delay dispose(t))

		def releasable[T](create: =>T)(implicit R:Releasable[T]):IoResource[T]	=
			IoResource.releasable(Io delay create)

		def lifecycle(before: =>Unit, after: =>Unit):IoResource[Unit]	=
			IoResource.lifecycle(Io delay before, Io delay after)

		def afterwards(dispose: =>Unit):IoResource[Unit]	=
			IoResource.afterwards(Io delay dispose)
	}

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit val IoResourceMonad:Monad[IoResource]	=
		new Monad[IoResource] {
			def pure[T](value:T):IoResource[T]										= IoResource pure value
			def flatMap[S,T](value:IoResource[S])(func:S=>IoResource[T]):IoResource[T]	= value flatMap func
		}

	implicit val IoResourceDelay:Delay[IoResource]	=
		new Delay[IoResource] {
			def delay[T](value: =>T):IoResource[T]	= IoResource delay value
		}
}

final case class IoResource[T](open:Io[(T,Io[Unit])]) {
	final def openVoid:Io[IoDisposer]	=
		open map { case (_, dt) => IoDisposer(dt) }

	final def useVoid():Io[Unit]	=
		use(_ => Io.unit)

	final def unsafeUse[U](handler:T=>U):Io[U]	=
		use(it => Io.delay(handler(it)))

	final def use[U](handler:T=>Io[U]):Io[U]	=
		for {
			tmp		<-	open
			(t, dt)	=	tmp
			xu		<-	handler(t).attempt
			xdt		<-	dt.attempt
			out		<-	(xu, xdt) match {
							case (Right(u),		Right(_))	=> Io.pure(u)
							case (Left(eu), 	Right(_))	=> Io.raise(eu)
							case (Right(_),		Left(edt))	=> Io.raise(edt)
							case (Left(eu),		Left(edt))	=> Io.raiseWithSecondary(eu, edt)
						}
		}
		yield out

	final def map[U](func:T=>U):IoResource[U]	=
		flatMap(func andThen IoResource.pure)

	final def ap[U,V](that:IoResource[U])(implicit ev:T=>U=>V):IoResource[V]	=
		map2(that) { (t,u) => t(u) }

	final def product[U](that:IoResource[U]):IoResource[(T,U)]	=
		map2(that) { (t,u) => (t,u) }

	final def map2[U,V](that:IoResource[U])(func:(T,U)=>V):IoResource[V]	=
		for { thisValue <- this; thatValue <- that }
		yield func(thisValue, thatValue)

	final def flatMap[U](func:T=>IoResource[U]):IoResource[U]	=
		IoResource {
			for {
				tmp		<-	open
				(t, dt)	= tmp
				xudu	<- 	func(t).open.attempt
				out		<-	xudu match {
								case Left(eu)		=>
									// second open failed, close first resource and fail now
									dt.attempt flatMap {
										case Left(edt)	=> Io.raiseWithSecondary[(U,Io[Unit])](eu, edt)
										case Right(_)	=> Io.raise[(U,Io[Unit])](eu)
									}
								case Right((u, du))	=>
									// both opens worked, close both resources later
									val dx:Io[Unit]	=
										for {
											xdu		<-	du.attempt
											xdt		<-	dt.attempt
											res		<-	(xdu, xdt) match {
															case (Left(edu),	Left(edt))	=> Io.raiseWithSecondary(edu, edt)
															case (Left(edu),	_)			=> Io.raise(edu)
															case (_,			Left(edt))	=> Io.raise(edt)
															case (_,			_)			=> Io.unit
													}
										}
										yield res
									Io.pure(u -> dx)
							}

			}
			yield out
		}
}
