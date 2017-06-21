package scutil.lang

import scala.util.{ Try, Success, Failure }
import scala.collection.generic.CanBuildFrom

import scutil.lang.tc._

object Validated extends ValidatedGenerated with ValidatedInstances {
	def good[E,T](value:T):Validated[E,T]	= Good(value)
	def bad[E,T](problems:E):Validated[E,T]	= Bad(problems)
	
	//------------------------------------------------------------------------------
	
	def fromEither[E,T](either:Either[E,T]):Validated[E,T]	=
			either match {
				case Left(x)	=> Bad(x)
				case Right(x)	=> Good(x)
			}
			
	def fromTried[E,T](tried:Tried[E,T]):Validated[E,T]	=
			tried match {
				case Fail(x)	=> Bad(x)
				case Win(x)		=> Good(x)
			}
			
	def fromTry[T](tryy:Try[T]):Validated[Throwable,T]	=
			tryy match {
				case Failure(x)	=> Bad(x)
				case Success(x)	=> Good(x)
			}
			
	//------------------------------------------------------------------------------
			
	def switch[E,T](ok:Boolean, problems: =>E, value: =>T):Validated[E,T]	=
			if (ok)	good(value)
			else	bad(problems)
		
	def goodOr[E,T](value:Option[T], problems: =>E):Validated[E,T]	=
			value match {
				case Some(x)	=> good(x)
				case None		=> bad(problems)
			}
		
	def badOr[E,T](problems:Option[E], value: =>T):Validated[E,T]	=
			problems match {
				case Some(x)	=> bad(x)
				case None		=> good(value)
			}
		
	def badOption[E](problems:Option[E]):Validated[E,Unit]	=
			badOr(problems, ())
		
	def goodCondition[E,T](condition:Boolean, problems: =>E):Validated[E,Unit]	=
			if (condition)	good(())
			else			bad(problems)
		
	def badCondition[E,T](condition:Boolean, problems: =>E):Validated[E,Unit]	=
			goodCondition(!condition, problems)
		
	//------------------------------------------------------------------------------
	
	/*
	// these we get from ValidatedGenerated
	
	def zip2[E:Semigroup,S1,S2](s1:Validated[E,S1], s2:Validated[E,S2]):Validated[E,(S1,S2)]	=
			s1 zip s2
			
	def zip3[E,S1,S2,S3](s1:Validated[E,S1], s2:Validated[E,S2], s3:Validated[E,S3]):Validated[E,(S1,S2,S3)]	=
			s1 zip s2 zip s3 map unarrow3
		
	def lift2[E,S1,S2,T](func:(S1,S2)=>T):(Validated[E,S1], Validated[E,S2])=>Validated[E,T]	=
			(s1, s2) => good(func.curried) ap s1 ap s2
	*/
}

sealed trait Validated[+E,+T] {
	def cata[X](bad:E=>X, good:T=>X):X	=
			this match {
				case Bad(x)		=> bad(x)
				case Good(x)	=> good(x)
			}
			
	//------------------------------------------------------------------------------
	
	def isGood:Boolean	=
			cata(Predicates.constFalse, Predicates.constTrue)
			
	def isBad:Boolean	=
			!isGood
		
	//------------------------------------------------------------------------------
	
	def exists(pred:Predicate[T]):Boolean	=
			cata(Predicates.constFalse, pred)
			
	def forall(pred:Predicate[T]):Boolean	=
			cata(Predicates.constTrue, pred)
			
	//------------------------------------------------------------------------------
	
	def iterator:Iterator[T]	=
			cata(_ => Iterator.empty, Iterator.single)
	
	def foreach(effect:Effect[T]):Unit	=
			cata(_ => (), effect)
		
	def map[U](func:T=>U):Validated[E,U]	=
			cata(Bad.apply, func andThen Good.apply)
			
	def flatMap[EE>:E,U](func:T=>Validated[EE,U]):Validated[EE,U]	=
			cata(Bad.apply, func)
			
	def flatten[EE>:E,U](implicit ev:T=>Validated[EE,U]):Validated[EE,U]	=
			flatMap(ev)
		
	/** function effect first */
	def ap[EE>:E:Semigroup,U,V](that:Validated[EE,U])(implicit ev:T=>U=>V):Validated[EE,V]	=
			that pa (this map ev)
		
	/** function effect first */
	def pa[EE>:E:Semigroup,U](that:Validated[EE,T=>U]):Validated[EE,U]	=
			(that zip this) map { case (t2u, t) => t2u(t) }
		
	def zip[EE>:E:Semigroup,U](that:Validated[EE,U]):Validated[EE,(T,U)]	=
			(this zipWith that)((_,_))
			
	def zipWith[EE>:E,U,V](that:Validated[EE,U])(func:(T,U)=>V)(implicit cc:Semigroup[EE]):Validated[EE,V]	=
			(this, that) match {
				case (Good(a),	Good(b))	=> Good(func(a, b))
				case (Bad(a),	Good(_))	=> Bad(a)
				case (Good(_),	Bad(b))		=> Bad(b)
				case (Bad(a),	Bad(b))		=> Bad(cc concat (a, b))
			}
	
			
	/** handy replacement for tried.toISeq.flatten abusing CanBuildFrom as a Zero typeclass */
	def flattenMany[U,CC[_]](implicit ev:T=>CC[U], cbf:CanBuildFrom[CC[U],U,CC[U]]):CC[U]	=
			// toOption.flattenMany
			this map ev match {
				case Good(cc)	=> cc
				case Bad(_)		=> cbf().result
			}
			
	//------------------------------------------------------------------------------

	def badOption:Option[E]	=
			cata(Some.apply, _ => None)
			
	//------------------------------------------------------------------------------
	
	def orElse[EE>:E,TT>:T](that:Validated[EE,TT])(implicit cc:Semigroup[EE]):Validated[EE,TT]	=
			(this, that) match {
				case (Good(a), _)		=> Good(a)
				case (Bad(a), Good(b))	=> Good(b)
				case (Bad(a), Bad(b))	=> Bad(cc concat (a, b))
			}
		
	def getOrElse[TT>:T](that: =>TT):TT	=
			cata(_ => that, identity)
		
	def getOrRescue[TT>:T](func:E=>TT):TT	=
			cata(func, identity)
		
	def getOrError(s: =>String):T	=
			getOrElse(sys error s)
		
	//------------------------------------------------------------------------------
	
	def rescue[TT>:T](func:PFunction[E,TT]):Validated[E,TT]	=
			cata(it => func(it) map Good.apply getOrElse Bad(it), Good.apply)
			
	def reject[EE>:E](func:PFunction[T,EE]):Validated[EE,T]	=
			cata(Bad.apply, it => func(it) map Bad.apply getOrElse Good(it))
			
	def guardByOr[EE>:E](func:Predicate[T], bad: =>EE):Validated[EE,T]	=
			cata(Bad.apply, it => if (func(it)) Good(it) else Bad(bad))
			
	def preventByOr[EE>:E](func:Predicate[T], bad: =>EE):Validated[EE,T]	=
			cata(Bad.apply, it => if (!func(it)) Good(it) else Bad(bad))
			
	def collapseOr[EE>:E,TT](func:PFunction[T,TT], bad: =>EE):Validated[EE,TT]	=
			cata(Bad.apply, it => func(it) map Good.apply getOrElse Bad(bad))
		
	def collectOr[EE>:E,TT](func:PartialFunction[T,TT], bad: =>EE):Validated[EE,TT]	=
			cata(Bad.apply, it => if (func isDefinedAt it) Good(func(it)) else Bad(bad))
			
	//------------------------------------------------------------------------------
	
	def goodEffect(effect:Effect[T]):this.type		= {
		this foreach effect
		this
	}
	
	def badEffect(effect:Effect[E]):this.type	= {
		badOption foreach effect
		this
	}
	
	//------------------------------------------------------------------------------
	
	def toTried:Tried[E,T]	=
			cata(Fail.apply, Win.apply)
			
	def toEither:Either[E,T]	=
			cata(Left.apply, Right.apply)
			
	def toOption:Option[T]	=
			cata(_ => None, Some.apply)
			
	def toISeq:ISeq[T]	=
			toVector
		
	def toList:List[T]	=
			cata(_ => Nil, List(_))
		
	def toVector:Vector[T]	=
			cata(_ => Vector.empty, Vector(_))
}

final case class Bad[E](problems:E)	extends Validated[E,Nothing]
final case class Good[T](value:T)	extends Validated[Nothing,T]

trait ValidatedInstances {
	implicit def ValidatedApplicative[S:Semigroup]:Applicative[Validated[S,?]]	=
			new Applicative[Validated[S,?]] {
				override def pure[A](it:A):Validated[S,A]											= Validated good it
				override def ap[A,B](it:Validated[S,A])(func:Validated[S,A=>B]):Validated[S,B]		= it pa func
			}
			
	implicit def ValidatedSemigroup[S:Semigroup,T]:Semigroup[Validated[S,T]]	=
			Semigroup instance (_ orElse _)
}