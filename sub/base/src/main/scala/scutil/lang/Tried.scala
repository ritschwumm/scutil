package scutil.lang

import scala.util.{ Try, Success, Failure }
import scala.collection.generic.CanBuildFrom

import scutil.lang.tc._

object Tried extends TriedGenerated with TriedInstances {
	def win[F,W](it:W):Tried[F,W]	= Win(it)
	def fail[F,W](it:F):Tried[F,W]	= Fail(it)
	
	//------------------------------------------------------------------------------
	
	def notNull[T](value:T):Tried[Null,T]	=
			if (value != null)	Win(value)
			else				Fail(null)
		
	//------------------------------------------------------------------------------
	
	def fromEither[F,W](either:Either[F,W]):Tried[F,W]	=
			either match {
				case Left(x)	=> Fail(x)
				case Right(x)	=> Win(x)
			}
			
	def fromValidated[F,W](validated:Validated[F,W]):Tried[F,W]	=
			validated match {
				case Bad(x)		=> Fail(x)
				case Good(x)	=> Win(x)
			}
			
	def fromTry[F,W](tryy:Try[W]):Tried[Throwable,W]	=
			tryy match {
				case Failure(x)	=> Fail(x)
				case Success(x)	=> Win(x)
			}
			
	//------------------------------------------------------------------------------
	
	def switch[F,W](condition:Boolean, falseFail: =>F, trueWin: =>W):Tried[F,W]	=
			if (condition)	Win(trueWin)
			else			Fail(falseFail)
		
	def winOr[F,W](value:Option[W], problem: =>F):Tried[F,W]	=
			value match {
				case Some(x)	=> Win(x)
				case None		=> Fail(problem)
			}
			
	def failOr[F,W](problem:Option[F], value: =>W):Tried[F,W]	=
			problem match {
				case Some(x)	=> Fail(x)
				case None		=> Win(value)
			}
			
	def failOption[F](problem:Option[F]):Tried[F,Unit]	=
			failOr(problem, ())
		
	def winCondition[F](condition:Boolean, problem: =>F):Tried[F,Unit]	=
			if (condition)	Win(())
			else			Fail(problem)
		
	def failCondition[F](condition:Boolean, problem: =>F):Tried[F,Unit]	=
			winCondition(!condition, problem)
		
	//------------------------------------------------------------------------------
	
	/*
	// these we get from TriedGenerated
	
	def zip2[E,S1,S2](s1:Tried[E,S1], s2:Tried[E,S2]):Tried[E,(S1,S2)]	=
			s1 zip s2
			
	def zip3[E,S1,S2,S3](s1:Tried[E,S1], s2:Tried[E,S2], s3:Tried[E,S3]):Tried[E,(S1,S2,S3)]	=
			s1 zip s2 zip s3 map unarrow3
			
	def lift2[E,S1,S2,T](func:(S1,S2)=>T):(Tried[E,S1], Tried[E,S2])=>Tried[E,T]	=
			(s1, s2) => win(func.curried) ap s1 ap s2
	*/
}

/** right biased Either, Try with parameterized error */
sealed trait Tried[+F,+W] {
	def cata[X](fail:F=>X, win:W=>X):X	=
			this match {
				case Win(x)		=> win(x)
				case Fail(x)	=> fail(x)
			}
	
	//------------------------------------------------------------------------------
		
	/** same as cata(identity,identity) but with improved type inference */
	def merge[U](implicit ev:this.type <:< Tried[U,U]):U	=
			ev(this) cata (identity, identity)
		
	def whereFail[FF,WW](that:Tried[FF,WW]):Tried[Where[F,FF],(W,WW)]	=
			(this, that) match {
				case (Fail(a), Fail(b))	=> Fail(Both(a, b))
				case (Fail(a), Win(_))	=> Fail(Here(a))
				case (Win(_),  Fail(b))	=> Fail(There(b))
				case (Win(a),  Win(b))	=> Win((a, b))
			}
		
	//------------------------------------------------------------------------------
	
	def isWin:Boolean	=
			cata(Predicates.constFalse, Predicates.constTrue)
		
	def isFail:Boolean	=
			!isWin
		
	//------------------------------------------------------------------------------
		
	def exists(pred:Predicate[W]):Boolean	=
			cata(Predicates.constFalse, pred)
		
	def forall(pred:Predicate[W]):Boolean	=
			cata(Predicates.constTrue, pred)
		
	//------------------------------------------------------------------------------
	
	def iterator:Iterator[W] =
			cata(_ => Iterator.empty, Iterator.single)
	
	def foreach(effect:Effect[W]):Unit	=
			cata(_ => (), effect)
		
	def map[X](func:W=>X):Tried[F,X]	=
			cata(Fail.apply, func andThen Win.apply)
		
	def flatMap[FF>:F,X](func:W=>Tried[FF,X]):Tried[FF,X]	=
			cata(Fail.apply, func)
		
	def flatten[FF>:F,X](implicit ev:W=>Tried[FF,X]):Tried[FF,X]	=
			flatMap(ev)
	
	/** function effect first */
	def ap[FF>:F,X,Y](that:Tried[FF,X])(implicit ev:W=>X=>Y):Tried[FF,Y]	=
			that pa (this map ev)
			
	/** function effect first */
	def pa[FF>:F,X](that:Tried[FF,W=>X]):Tried[FF,X]	=
			that cata(
				f	=> Fail(f),
				f	=> this cata (
					v	=> Fail(v),
					v	=> Win(f(v))
				)
			)
			
	def zip[FF>:F,X](that:Tried[FF,X]):Tried[FF,(W,X)]	=
			(this, that) match {
				case (Win(a),	Win(b))		=> Win((a, b))
				case (Fail(a),	Win(_))		=> Fail(a)
				case (Win(_),	Fail(b))	=> Fail(b)
				case (Fail(a),	Fail(b))	=> Fail(a)
			}
			
	def zipWith[FF>:F,X,Y](that:Tried[FF,X])(func:(W,X)=>Y):Tried[FF,Y]	=
			this zip that map func.tupled
		
	/** handy replacement for tried.toISeq.flatten abusing CanBuildFrom as a Zero typeclass */
	def flattenMany[U,CC[_]](implicit ev:W=>CC[U], cbf:CanBuildFrom[CC[U],U,CC[U]]):CC[U]	=
			// toOption.flattenMany
			this map ev match {
				case Win(cc)	=> cc
				case Fail(_)	=> cbf().result
			}
			
	//------------------------------------------------------------------------------
			
	def swap:Tried[W,F]	=
			cata(Win.apply,	Fail.apply)
		
	def withSwapped[FX,WX](func:Tried[W,F]=>Tried[WX,FX]):Tried[FX,WX]	=
			func(swap).swap
		
	//------------------------------------------------------------------------------
	
	def bimap[FX,WX](failFunc:F=>FX, winFunc:W=>WX):Tried[FX,WX]	=
			cata(failFunc andThen Fail.apply, winFunc andThen Win.apply)
		
	def mapFail[FX](func:F=>FX):Tried[FX,W]	=
			cata(func andThen Fail.apply, Win.apply)
		
	def flatMapFail[FX,WW>:W](func:F=>Tried[FX,WW]):Tried[FX,WW]	=
			cata(func, Win.apply)
		
	//------------------------------------------------------------------------------
	
	def orElse[FF>:F,WW>:W](that: =>Tried[FF,WW]):Tried[FF,WW]	=
			cata(_ => that, Win.apply)
		
	def getOrElse[WW>:W](that: =>WW):WW	=
			cata(_ => that, identity)
		
	def getOrRescue[WW>:W](func:F=>WW):WW	=
			cata(func, identity)
		
	def getOrError(s: =>String):W	=
			getOrElse(sys error s)
		
	//------------------------------------------------------------------------------
	
	def rescue[WW>:W](func:PFunction[F,WW]):Tried[F,WW]	=
			cata(it => func(it) map Win.apply getOrElse Fail(it), Win.apply)
		
	def rescuePartial[WW>:W](func:PartialFunction[F,WW]):Tried[F,WW]	=
			rescue(func.lift)
		
	def reject[FF>:F](func:PFunction[W,FF]):Tried[FF,W]	=
			cata(Fail.apply, it => func(it) map Fail.apply getOrElse Win(it))
		
	def rejectPartial[FF>:F](func:PartialFunction[W,FF]):Tried[FF,W]	=
			reject(func.lift)
		
	def guardByOr[FF>:F](func:Predicate[W], fail: =>FF):Tried[FF,W]	=
			cata(Fail.apply, it => if (func(it)) Win(it) else Fail(fail))
			
	def preventByOr[FF>:F](func:Predicate[W], fail: =>FF):Tried[FF,W]	=
			cata(Fail.apply, it => if (!func(it)) Win(it) else Fail(fail))
			
	def collapseOr[FF>:F,WW](func:PFunction[W,WW], fail: =>FF):Tried[FF,WW]	=
			cata(Fail.apply, it => func(it) map Win.apply getOrElse Fail(fail))
		
	def collectOr[FF>:F,WW](func:PartialFunction[W,WW], fail: =>FF):Tried[FF,WW]	=
			cata(Fail.apply, it => if (func isDefinedAt it) Win(func(it)) else Fail(fail))
	
	//------------------------------------------------------------------------------
		
	def throwThrowable(implicit ev:F=>Throwable):W	=
			cata(throw _, identity)
	
	def throwException(implicit ev:F=>Exception):W	=
			cata(throw _, identity)
		
	//------------------------------------------------------------------------------
	
	def winEffect(effect:W=>Unit):this.type	= {
		this foreach effect
		this
	}
	
	def failEffect(effect:F=>Unit):this.type	= {
		this.swap foreach effect
		this
	}
	
	//------------------------------------------------------------------------------
	
	def toTry(implicit ev:F=>Throwable):Try[W]	=
			cata(Failure(_), Success(_))
	
	def toEither:Either[F,W]	=
			cata(Left.apply, Right.apply)
		
	def toValidated:Validated[F,W]	=
			Validated fromTried this
		
	def toOption:Option[W]	=
			cata(_ => None, Some.apply)
		
	def toISeq:ISeq[W]	=
			toVector
		
	def toList:List[W]	=
			cata(_ => Nil, List(_))
		
	def toVector:Vector[W]	=
			cata(_ => Vector.empty, Vector(_))
}

final case class Win[F,W](value:W)	extends Tried[F,W]
final case class Fail[F,W](value:F)	extends Tried[F,W]

trait TriedInstances {
	implicit def TriedMonad[S]:Monad[Tried[S,?]]	=
			new Monad[Tried[S,?]] {
				override def pure[A](it:A):Tried[S,A]									= Tried win it
				override def map[A,B](it:Tried[S,A])(func:A=>B):Tried[S,B]				= it map func
				override def flatMap[A,B](it:Tried[S,A])(func:A=>Tried[S,B]):Tried[S,B]	= it flatMap func
			}
			
	implicit def TriedSemigroup[S,T]:Semigroup[Tried[S,T]]	=
			Semigroup instance (_ orElse _)
}
