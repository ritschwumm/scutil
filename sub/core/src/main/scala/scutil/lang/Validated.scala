package scutil.lang

import scala.util.{ Try, Success, Failure }
import scala.collection.generic.CanBuildFrom

object Validated extends ValidatedGenerated {
	def bad[E,T](problems:Nes[E]):Validated[E,T]	= Bad(problems)
	def good[E,T](value:T):Validated[E,T]			= Good(value)
	
	def bad1[E,T](problem:E):Validated[E,T]			= bad(Nes single problem)
	
	//------------------------------------------------------------------------------
	
	def fromEither[E,T](either:Either[Nes[E],T]):Validated[E,T]	=
			either match {
				case Left(es)	=> Bad(es)
				case Right(x)	=> Good(x)
			}
			
	def fromTried[E,T](tried:Tried[Nes[E],T]):Validated[E,T]	=
			tried match {
				case Fail(x)	=> Bad(x)
				case Win(x)		=> Good(x)
			}
			
	def fromTry[T](tryy:Try[T]):Validated[Throwable,T]	=
			tryy match {
				case Failure(x)	=> Bad(Nes single x)
				case Success(x)	=> Good(x)
			}
			
	//------------------------------------------------------------------------------
			
	def switch[E,T](ok:Boolean, problems: =>Nes[E], value: =>T):Validated[E,T]	=
			if (ok)	good(value)
			else	bad(problems)
		
	def switch1[E,T](ok:Boolean, problem: =>E, value: =>T):Validated[E,T]	=
			if (ok)	good(value)
			else	bad1(problem)
		
	def goodOr[E,T](value:Option[T], problems: =>Nes[E]):Validated[E,T]	=
			value match {
				case Some(x)	=> good(x)
				case None		=> bad(problems)
			}
		
	def badOr[E,T](problems:Option[Nes[E]], value: =>T):Validated[E,T]	=
			problems match {
				case Some(x)	=> bad(x)
				case None		=> good(value)
			}
		
	def badISeq[E](problems:ISeq[E]):Validated[E,Unit]	=
			badOr(Nes fromISeq problems, ())
		
	def badOption[E](problems:Option[Nes[E]]):Validated[E,Unit]	=
			badOr(problems, ())
		
	def goodCondition[E,T](condition:Boolean, problems: =>Nes[E]):Validated[E,Unit]	=
			if (condition)	good(())
			else			bad(problems)
		
	def badCondition[E,T](condition:Boolean, problems: =>Nes[E]):Validated[E,Unit]	=
			goodCondition(!condition, problems)
		
	//------------------------------------------------------------------------------
	
	def zip2[E,S1,S2](s1:Validated[E,S1], s2:Validated[E,S2]):Validated[E,(S1,S2)]	=
			s1 zip s2
		
	/*
	// these we get from ValidatedGenerated
			
	def zip3[E,S1,S2,S3](s1:Validated[E,S1], s2:Validated[E,S2], s3:Validated[E,S3]):Validated[E,(S1,S2,S3)]	=
			s1 zip s2 zip s3 map Tuples.runcurry3
		
	def lift2[E,S1,S2,T](func:(S1,S2)=>T):(Validated[E,S1], Validated[E,S2])=>Validated[E,T]	=
			(s1, s2) => good(func.curried) ap s1 ap s2
	*/
}

sealed trait Validated[+E,+T] {
	def cata[X](bad:Nes[E]=>X, good:T=>X):X	=
			this match {
				case Bad(es)	=> bad(es)
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
			
	/** note this is nort consitent with ap */
	def flatMap[EE>:E,U](func:T=>Validated[EE,U]):Validated[EE,U]	=
			cata(Bad.apply, func)
			
	/** note this is nort consitent with ap */
	def flatten[EE>:E,U](implicit ev:T=>Validated[EE,U]):Validated[EE,U]	=
			flatMap(ev)
		
	/** error in that comes first */
	def pa[EE>:E,U](that:Validated[EE,T=>U]):Validated[EE,U]	=
			(that zip this) map { case (t2u, t) => t2u(t) }
		
	/** error in this comes first */
	def ap[EE>:E,U,V](that:Validated[EE,U])(implicit ev:T=>U=>V):Validated[EE,V]	=
			(this zip that) map { case (fuv, u) => fuv(u) }
		
	def zip[EE>:E,U](that:Validated[EE,U]):Validated[EE,(T,U)]	=
			(this, that) match {
				case (Good(a),	Good(b))	=> Good((a, b))
				case (Bad(a),	Good(_))	=> Bad(a)
				case (Good(_),	Bad(b))		=> Bad(b)
				case (Bad(a),	Bad(b))		=> Bad(a ++ b)
			}
			
	def zipWith[EE>:E,U,V](that:Validated[EE,U])(func:(T,U)=>V):Validated[EE,V]	=
			this zip that map func.tupled
			
	/** handy replacement for tried.toISeq.flatten abusing CanBuildFrom as a Zero typeclass */
	def flattenMany[U,CC[_]](implicit ev:T=>CC[U], cbf:CanBuildFrom[CC[U],U,CC[U]]):CC[U]	=
			// toOption.flattenMany
			this map ev match {
				case Good(cc)	=> cc
				case Bad(_)		=> cbf().result
			}
			
	//------------------------------------------------------------------------------

	def badOption:Option[Nes[E]]	=
			cata(Some.apply, _ => None)
			
	def badProblems:ISeq[E]	=
			cata(_.toVector, _ => Vector.empty)
				
	//------------------------------------------------------------------------------
	
	def orElse[EE>:E,TT>:T](that:Validated[EE,TT]):Validated[EE,TT]	=
			cata(
				thisProblems	=> that cata (
					thatProblems	=> 	Bad(
						thisProblems ++ thatProblems
					),
					Good.apply
				),
				Good.apply
			)
		
	def getOrElse[TT>:T](that: =>TT):TT	=
			cata(_ => that, identity)
		
	def getOrRescue[TT>:T](func:Nes[E]=>TT):TT	=
			cata(func, identity)
		
	def getOrError(s: =>String):T	=
			getOrElse(sys error s) 
		
	//------------------------------------------------------------------------------
	
	def rescue[TT>:T](func:PFunction[Nes[E],TT]):Validated[E,TT]	=
			cata(it => func(it) map Good.apply getOrElse Bad(it), Good.apply)
			
	def reject[EE>:E](func:PFunction[T,Nes[EE]]):Validated[EE,T]	=
			cata(Bad.apply, it => func(it) map Bad.apply getOrElse Good(it))
			
	def guardByOr[EE>:E](func:Predicate[T], bad: =>Nes[EE]):Validated[EE,T]	=
			cata(Bad.apply, it => if (func(it)) Good(it) else Bad(bad))
			
	def preventByOr[EE>:E](func:Predicate[T], bad: =>Nes[EE]):Validated[EE,T]	=
			cata(Bad.apply, it => if (!func(it)) Good(it) else Bad(bad))
			
	def collapseOr[EE>:E,TT](func:PFunction[T,TT], bad: =>Nes[EE]):Validated[EE,TT]	=
			cata(Bad.apply, it => func(it) map Good.apply getOrElse Bad(bad))
		
	def collectOr[EE>:E,TT](func:PartialFunction[T,TT], bad: =>Nes[EE]):Validated[EE,TT]	=
			cata(Bad.apply, it => if (func isDefinedAt it) Good(func(it)) else Bad(bad))
			
	//------------------------------------------------------------------------------
	
	def goodEffect(effect:Effect[T]):this.type		= {
		this foreach effect
		this
	}
	
	def badEffect(effect:Effect[Nes[E]]):this.type	= {
		badOption foreach effect
		this
	}
	
	//------------------------------------------------------------------------------
	
	def toTried:Tried[Nes[E],T]	=
			cata(Fail.apply, Win.apply)
			
	def toEither:Either[Nes[E],T]	=
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

final case class Bad[E](problems:Nes[E])	extends Validated[E,Nothing]
final case class Good[T](value:T)			extends Validated[Nothing,T]
