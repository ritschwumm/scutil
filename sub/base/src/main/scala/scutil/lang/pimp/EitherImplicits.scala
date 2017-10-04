package scutil.lang.pimp

import scala.util.{ Try, Success, Failure }
import scala.collection.generic.CanBuildFrom

import scutil.lang._
import scutil.lang.tc._

object EitherImplicits extends EitherImplicits

trait EitherImplicits {
	implicit final class EitherCompanionImplicits(peer:Either.type) extends EitherGenerated {
		def left[L,R](it:L):Either[L,R]		= Left(it)
		def right[L,R](it:R):Either[L,R]	= Right(it)
		
		//------------------------------------------------------------------------------
		
		def notNull[T<:AnyRef](it:T):Either[Null,T]	=
				if (it eq null)	Left(null)
				else			Right(it)
			
		// Either.cond with flipped arguments
		def switch[L,R](condition:Boolean, falseLeft: =>L, trueRight: =>R):Either[L,R]	=
				Either cond (condition, trueRight, falseLeft)
	}
	
	implicit final class EitherExt[L,R](peer:Either[L,R]) {
		def cata[U](left:L=>U, right:R=>U):U	= peer fold (left, right)
		
		//------------------------------------------------------------------------------
		
		// exists: merge
		
		def whereFail[LL,RR](that:Either[LL,RR]):Either[Where[L,LL],(R,RR)]	=
				(peer, that) match {
					case (Left(a),	Left(b))	=> Left(Both(a, b))
					case (Left(a),	Right(_))	=> Left(Here(a))
					case (Right(_),	Left(b))	=> Left(There(b))
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
		
		def flatten[LL>:L,X](implicit ev:R=>Either[LL,X]):Either[LL,X]	=
				peer flatMap ev
		
		/** function effect first */
		def ap[LL>:L,X,Y](that:Either[LL,X])(implicit ev:R=>X=>Y):Either[LL,Y]	=
				that pa (peer map ev)
				
		/** function effect first */
		def pa[LL>:L,X](that:Either[LL,R=>X]):Either[LL,X]	=
				that match {
					case Left(l1)	=> Left(l1)
					case Right(r1)	=>
						peer match {
							case Left(l2)	=> Left(l2)
							case Right(r2)	=> Right(r1(r2))
						}
				}
			
		def zip[LL>:L,X](that:Either[LL,X]):Either[LL,(R,X)]	=
				(peer, that) match {
					case (Right(a),	Right(b))	=> Right((a, b))
					case (Left(a),	Right(_))	=> Left(a)
					case (Right(_),	Left(b))	=> Left(b)
					case (Left(a),	Left(b))	=> Left(a)
				}
			
		def zipWith[LL>:L,X,Y](that:Either[LL,X])(func:(R,X)=>Y):Either[LL,Y]	=
				peer zip that map func.tupled
			
		/** handy replacement for tried.toISeq.flatten abusing CanBuildFrom as a Zero typeclass */
		def flattenMany[U,CC[_]](implicit ev:R=>CC[U], cbf:CanBuildFrom[CC[U],U,CC[U]]):CC[U]	=
				// toOption.flattenMany
				peer map ev match {
					case Left(_)	=> cbf().result
					case Right(cc)	=> cc
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
		
		def mapLeft[LL](func:L=>LL):Either[LL,R]	=
				peer match {
					case Left(x)	=> Left(func(x))
					case Right(x)	=> Right(x)
				}
				
		def flatMapLeft[LL,RR>:R](func:L=>Either[LL,RR]):Either[LL,RR]	=
				peer match {
					case Left(x)	=> func(x)
					case Right(x)	=> Right(x)
				}
				
		def flattenLeft[LL,RR>:R](implicit ev:L=>Either[LL,RR]):Either[LL,RR]	=
				flatMapLeft(ev)
		
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
				peer getOrElse (sys error s)
			
		//------------------------------------------------------------------------------
		
		// TODO use pattern matching instead of cata here
		
		def rescue[RR>:R](func:PFunction[L,RR]):Either[L,RR]	=
				cata(it => func(it) map Right.apply getOrElse Left(it), Right.apply)
		
		def rescuePartial[RR>:R](func:PartialFunction[L,RR]):Either[L,RR]	=
				rescue(func.lift)
			
		def reject[LL>:L](func:PFunction[R,LL]):Either[LL,R]	=
				cata(Left.apply, it => func(it) map Left.apply getOrElse Right(it))
			
		def rejectPartial[LL>:L](func:PartialFunction[R,LL]):Either[LL,R]	=
				reject(func.lift)
			
		def guardByOr[LL>:L](func:Predicate[R], fail: =>LL):Either[LL,R]	=
				cata(Left.apply, it => if (func(it)) Right(it) else Left(fail))
				
		def preventByOr[LL>:L](func:Predicate[R], fail: =>LL):Either[LL,R]	=
				cata(Left.apply, it => if (!func(it)) Right(it) else Left(fail))
				
		def collapseOr[LL>:L,RR](func:PFunction[R,RR], fail: =>LL):Either[LL,RR]	=
				cata(Left.apply, it => func(it) map Right.apply getOrElse Left(fail))
			
		def collectOr[LL>:L,RR](func:PartialFunction[R,RR], fail: =>LL):Either[LL,RR]	=
				cata(Left.apply, it => if (func isDefinedAt it) Right(func(it)) else Left(fail))
		
		//------------------------------------------------------------------------------
	
		def throwThrowable(implicit ev:L=>Throwable):R	=
				cata(throw _, identity)
		
		def throwException(implicit ev:L=>Exception):R	=
				cata(throw _, identity)
				
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
		
		def toTry(implicit ev:L=>Throwable):Try[R]	=
				peer match {
					case Left(x)	=> Failure(x)
					case Right(x)	=> Success(x)
				}
				
		def toValidated:Validated[L,R]	=
				peer match {
					case Left(x)	=> Bad(x)
					case Right(x)	=> Good(x)
				}
			
		// exists: toOption
		
		def toISeq:ISeq[R]	=
				toVector
		
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
		
		def toEitherT[F[_]](implicit M:Applicative[F]):EitherT[F,L,R]	=
				EitherT fromEither peer
	}
}
