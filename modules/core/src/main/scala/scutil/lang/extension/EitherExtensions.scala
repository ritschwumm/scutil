package scutil.lang.extension

import scutil.lang.*
import scutil.lang.tc.*

object EitherExtensions {
	extension (peer:Either.type) {
		def unit[L]:Either[L,Unit]			= Right(())

		def right[L,R](it:R):Either[L,R]	= Right(it)
		def left[L,R](it:L):Either[L,R]		= Left(it)

		//------------------------------------------------------------------------------

		def notNull[T<:AnyRef](it:T):Either[Null,T]	=
			if (it eq null)	Left(null)
			else			Right(it)

		// Either.cond with flipped arguments
		def switch[L,R](condition:Boolean, falseLeft: =>L, trueRight: =>R):Either[L,R]	=
			Either.cond(condition, trueRight, falseLeft)
	}

	extension [L,R](peer:Either[L,R]) {
		def cata[U](left:L=>U, right:R=>U):U	= peer.fold(left, right)

		//------------------------------------------------------------------------------

		// exists: merge

		def iorFail[LL,RR](that:Either[LL,RR]):Either[Ior[L,LL],(R,RR)]	=
			(peer, that) match {
				case (Left(a),	Left(b))	=> Left(Ior.both(a, b))
				case (Left(a),	Right(_))	=> Left(Ior.left(a))
				case (Right(_),	Left(b))	=> Left(Ior.right(b))
				case (Right(a),	Right(b))	=> Right((a, b))
			}

		//------------------------------------------------------------------------------

		// exists: isLeft
		// exists: isRight
		// exists: exists
		// exists: forall

		//------------------------------------------------------------------------------

		def iterator:Iterator[R]	= peer.toSeq.iterator

		// exists: foreach
		// exists: map
		// exists: flatMap
		// exists: flatten

		/*
		// NOTE we get these from applicative syntax

		def flatten[LL>:L,X](using ev: R <:< Either[LL,X]):Either[LL,X]	=
			peer.flatMap(ev)

		// * function effect first
		def ap[LL>:L,X,Y](that:Either[LL,X])(using ev: R <:< (X=>Y)):Either[LL,Y]	=
			that.pa(peer.map(ev))

		// * function effect first
		def pa[LL>:L,X](that:Either[LL,R=>X]):Either[LL,X]	=
			that match {
				case Left(l1)	=> Left(l1)
				case Right(r1)	=>
					peer match {
						case Left(l2)	=> Left(l2)
						case Right(r2)	=> Right(r1(r2))
					}
			}

		def map2[LL>:L,X,Y](that:Either[LL,X])(func:(R,X)=>Y):Either[LL,Y]	=
			peer.zip(that).map(func.tupled)
		*/

		def zip[LL>:L,X](that:Either[LL,X]):Either[LL,(R,X)]	=
			(peer, that) match {
				case (Right(a),	Right(b))	=> Right((a, b))
				case (Left(a),	Right(_))	=> Left(a)
				case (Right(_),	Left(b))	=> Left(b)
				case (Left(a),	Left(b))	=> Left(a)
			}

		//------------------------------------------------------------------------------

		// exists: swap

		def withSwapped[LL,RR](func:Either[R,L]=>Either[RR,LL]):Either[LL,RR]	=
			func(peer.swap).swap

		def bimap[LL,RR](leftFunc:L=>LL, rightFunc:R=>RR):Either[LL,RR]	=
			peer match {
				case Left(x)	=> Left(leftFunc(x))
				case Right(x)	=> Right(rightFunc(x))
			}

		//------------------------------------------------------------------------------

		def leftMap[LL](func:L=>LL):Either[LL,R]	=
			peer match {
				case Left(x)	=> Left(func(x))
				case Right(x)	=> Right(x)
			}

		def leftFlatMap[LL,RR>:R](func:L=>Either[LL,RR]):Either[LL,RR]	=
			peer match {
				case Left(x)	=> func(x)
				case Right(x)	=> Right(x)
			}

		def leftFlatten[LL,RR>:R](using ev: L <:< Either[LL,RR]):Either[LL,RR]	=
			leftFlatMap(ev)

		def leftToOption:Option[L]	=
			peer.swap.toOption

		//------------------------------------------------------------------------------

		def orElse[LL>:L,RR>:R](that: =>Either[LL,RR]):Either[LL,RR]	=
			peer match {
				case Left(_)	=> that
				case Right(x)	=> Right(x)
			}

		// exists: getOrElse

		def getOrRescue[RR>:R](func:L=>RR):RR	=
			peer match {
				case Left(x)	=> func(x)
				case Right(x)	=> x
			}

		def getOrError(s: =>String):R	=
			peer.getOrElse{ sys error s }

		//------------------------------------------------------------------------------

		def rescue[RR>:R](func:L=>Option[RR]):Either[L,RR]	=
			peer match {
				case Left(x)	=> func(x).map(Right.apply).getOrElse(Left(x))
				case Right(x)	=> Right(x)
			}

		def rescuePartial[RR>:R](func:PartialFunction[L,RR]):Either[L,RR]	=
			rescue(func.lift)

		def reject[LL>:L](func:R=>Option[LL]):Either[LL,R]	=
			peer match {
				case Left(x)	=> Left(x)
				case Right(x)	=> func(x).map(Left.apply).getOrElse(Right(x))
			}

		def rejectPartial[LL>:L](func:PartialFunction[R,LL]):Either[LL,R]	=
			reject(func.lift)

		def rightByOr[LL>:L](func:Predicate[R], fail: =>LL):Either[LL,R]	=
			peer match {
				case Left(x)	=> Left(x)
				case Right(x)	=> if (func(x)) Right(x) else Left(fail)
			}

		def rightNotByOr[LL>:L](func:Predicate[R], fail: =>LL):Either[LL,R]	=
			peer match {
				case Left(x)	=> Left(x)
				case Right(x)	=> if (!func(x)) Right(x) else Left(fail)
			}

		def collapseOr[LL>:L,RR](func:R=>Option[RR], fail: =>LL):Either[LL,RR]	=
			peer match {
				case Left(x)	=> Left(x)
				case Right(x)	=> func(x).map(Right.apply).getOrElse(Left(fail))
			}

		def collectOr[LL>:L,RR](func:PartialFunction[R,RR], fail: =>LL):Either[LL,RR]	=
			collapseOr(func.lift, fail)

		//------------------------------------------------------------------------------

		def throwMessage(using ev:L <:< String):R	=
			peer match {
				case Left(x)	=> throw new RuntimeException(x)
				case Right(x)	=> x
			}

		def throwThrowable(using ev:L <:< Throwable):R	=
			peer match {
				case Left(x)	=> throw x
				case Right(x)	=> x
			}

		def throwException(using ev:L <:< Exception):R	=
			peer match {
				case Left(x)	=> throw x
				case Right(x)	=> x
			}

		def getOrThrow(func:L=>Throwable):R	=
			peer match {
				case Left(x)	=> throw func(x)
				case Right(x)	=> x
			}

		//------------------------------------------------------------------------------

		def leftEffect(fx:L=>Unit):Either[L,R]	= {
			peer.swap foreach fx
			peer
		}

		def rightEffect(fx:R=>Unit):Either[L,R]	= {
			peer foreach fx
			peer
		}

		//------------------------------------------------------------------------------

		def toIor:Ior[L,R]	=
			peer match {
				case Left(a)	=> Ior.left(a)
				case Right(b)	=> Ior.right(b)
			}

		/*
		def toTry(using ev:L <:< Throwable):Try[R]	=
			peer match {
				case Left(x)	=> Failure(x)
				case Right(x)	=> Success(x)
			}
		*/

		def toValidated:Validated[L,R]	=
			peer match {
				case Left(x)	=> Validated.invalid(x)
				case Right(x)	=> Validated.valid(x)
			}

		// exists: toOption
		// exists: toSeq

		def toList:List[R]	=
			peer match {
				case Left(_)	=> Nil
				case Right(x)	=> List(x)
			}

		def toVector:Vector[R]	=
			peer match {
				case Left(_)	=> Vector.empty
				case Right(x)	=> Vector(x)
			}

		//------------------------------------------------------------------------------

		def toEitherT[F[_]](using M:Applicative[F]):EitherT[F,L,R]	=
			EitherT.fromEither(peer)
	}
}
