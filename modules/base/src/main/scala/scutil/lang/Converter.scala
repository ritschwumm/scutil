package scutil.lang

import scutil.base.implicits._
import scutil.lang.tc._

object Converter extends ConverterGenerated with ConverterInstances {
	def identity[E,T]:Converter[E,T,T]	=
			Converter { it =>
				Validated good it
			}
			
	def constant[E,S,T](it:T):Converter[E,S,T]	=
			Converter { _ =>
				Validated good it
			}
			
	def fail[E,S,T](it:E):Converter[E,S,T]	=
			Converter { _ =>
				Validated bad it
			}
	
	def total[E,S,T](func:S=>T):Converter[E,S,T]	=
			Converter { it =>
				Validated good func(it)
			}
			
	def optional[E,S,T](func:PFunction[S,T], bad: =>E):Converter[E,S,T]	=
			Converter { it =>
				func(it) toGood bad
			}
			
	def partial[E,S,T](func:PartialFunction[S,T], bad: =>E):Converter[E,S,T]	=
			optional(func.lift, bad)
			
	def rejecting[E,T](func:PFunction[T,E]):Converter[E,T,T]	=
			Converter { it =>
				func(it) toBad it
			}
			
	def fromEitherFunc[E,S,T](func:S=>Either[E,T]):Converter[E,S,T]	=
			Converter(func andThen (_.toValidated))
		
	def required[E,S,T](base:Converter[E,S,Option[T]], bad: =>E):Converter[E,S,T]	=
			Converter { input =>
				base convert input flatMap { _ toGood bad }
			}
			
	//------------------------------------------------------------------------------
	
	/*
	// these we get from ConverterGenerated
	
	def zip2[E:Semigroup,S,T1, T2](t1:Converter[E,S,T1], t2:Converter[E,S,T2]):Converter[E,S,(T1, T2)]	=
			Converter { it => Validated zip2 (t1 convert it, t2 convert it) }
	
	def lift2[E:Semigroup,R,S1, S2,T](func:(S1, S2)=>T):(Converter[E,R,S1], Converter[E,R,S2])=>Converter[E,R,T]	=
			(c1, c2)	=> Converter { it => Validated lift2 func apply (c1 convert it, c2 convert it) }
	
	def map2[E:Semigroup,R,S1, S2,T](c1:Converter[E,R,S1], c2:Converter[E,R,S2]):((S1, S2)=>T)=>Converter[E,R,T]	=
			func => Converter { it => Validated map2 (c1 convert it, c2 convert it) apply func }
	*/
	
	//------------------------------------------------------------------------------
	
	def sum[E,S,T](subs:ISeq[PFunction[S,Validated[E,T]]], bad: =>E):Converter[E,S,T]	=
			Converter { it =>
				subs
				.collapseMapFirst	{ _ apply it }
				.getOrElse			(Validated bad bad)
			}
}

// Kleisli[Validated[E,_],S,T]
final case class Converter[E,S,T](convert:S=>Validated[E,T]) {
	def apply(s:S):Validated[E,T]	= convert(s)
	
	def varyIn[SS<:S]:Converter[E,SS,T]		= Converter(convert)
	def varyOut[TT>:T]:Converter[E,S,TT]	= Converter(convert)
	def varyError[EE>:E]:Converter[EE,S,T]	= Converter(convert)
	
	//------------------------------------------------------------------------------
	
	def andThen[U](that:Converter[E,T,U]):Converter[E,S,U]	=
			Converter { it =>
				this convert it flatMap that.convert
			}
	
	def compose[R](that:Converter[E,R,S]):Converter[E,R,T]	=
			that andThen this
		
	def >=>[U](that:Converter[E,T,U]):Converter[E,S,U]	=
			this andThen that
	
	def <=<[R](that:Converter[E,R,S]):Converter[E,R,T]	=
			this compose that
	
	def map[U](func:T=>U):Converter[E,S,U]		=
			Converter { it =>
				convert(it) map func
			}
			
	def flatMap[U](func:T=>Converter[E,S,U]):Converter[E,S,U]	=
			Converter { it =>
				this convert it map func flatMap { _ convert it }
			}
		
	def contraMap[R](func:R=>S):Converter[E,R,T]	=
			Converter { it =>
				convert(func(it))
			}
			
	def tag[U](it:U):Converter[E,S,U]	=
			map(constant(it))
		
	def pa[U](that:Converter[E,S,T=>U])(implicit cc:Semigroup[E]):Converter[E,S,U]	=
			Converter { it =>
				(this convert it) pa (that convert it)
			}
	
	def ap[U,V](that:Converter[E,S,U])(implicit ev:T=>U=>V, cc:Semigroup[E]):Converter[E,S,V]	=
			that pa (this map ev)
	
	def zip[U](that:Converter[E,S,U])(implicit cc:Semigroup[E]):Converter[E,S,(T,U)] =
			Converter { it	=>
				(this convert it) zip (that convert it)
			}
			
	def zipWith[U,V](that:Converter[E,S,U])(func:(T,U)=>V)(implicit cc:Semigroup[E]):Converter[E,S,V] =
			Converter { it	=>
				((this convert it) zipWith (that convert it))(func)
			}
			
	def coZip[SS](that:Converter[E,SS,T]):Converter[E,Either[S,SS],T]	=
			Converter {
				case Left(x)	=> this convert x
				case Right(x)	=> that convert x
			}
			
	def orElse(that:Converter[E,S,T])(implicit cc:Semigroup[E]):Converter[E,S,T]	=
			Converter { (it:S) =>
				(this convert it) orElse (that convert it)
			}
			
	def either[SS,TT](that:Converter[E,SS,TT]):Converter[E,Either[S,SS],Either[T,TT]]	=
			Converter {
				case Left(x)	=> this convert x map (Left(_))
				case Right(x)	=> that convert x map (Right(_))
			}
			
	def pair[SS,TT](that:Converter[E,SS,TT]):Converter[E,(S,SS),(T,TT)]	=
			Converter { case (s,ss) =>
				this convert s flatMap { t =>
					that convert ss map { tt =>
						(t, tt)
					}
				}
			}
			
	//------------------------------------------------------------------------------
			
	def liftFirst[X]:Converter[E,(S,X),(T,X)]	=
			Converter { case (s,x) =>
				convert(s) map { (_, x) }
			}
			
	def liftSecond[X]:Converter[E,(X,S),(X,T)]	=
			Converter { case (x,s) =>
				convert(s) map { (x, _) }
			}
			
	def liftOption:Converter[E,Option[S],Option[T]]	=
			Converter { it =>
				(it map convert).sequenceValidated
			}
		
	// TODO liftTraversable
			
	def liftISeq(implicit cc:Semigroup[E]):Converter[E,ISeq[S],ISeq[T]]	=
			Converter { it =>
				it traverseValidated convert
			}
			
	def liftList(implicit cc:Semigroup[E]):Converter[E,List[S],List[T]]	=
			Converter { it =>
				it traverseValidated convert
			}
			
	def liftVector(implicit cc:Semigroup[E]):Converter[E,Vector[S],Vector[T]]	=
			Converter { it =>
				it traverseValidated convert
			}
			
	def liftNes(implicit cc:Semigroup[E]):Converter[E,Nes[S],Nes[T]]	=
			Converter { it =>
				// NOTE this uses the Traverse instance instead of a native implementation
				it traverse convert
			}
			
	def liftSet(implicit cc:Semigroup[E]):Converter[E,Set[S],Set[T]]	=
			Converter { it =>
				it traverseValidated convert
			}
			
	//------------------------------------------------------------------------------
	
	def toEitherFunc:S=>Either[E,T]	=
			convert andThen (_.toEither)
}

trait ConverterInstances {
	// TODO restrict this to applicative?
	implicit def ConverterMonad[E,S]:Monad[Converter[E,S,?]]	=
			new Monad[Converter[E,S,?]] {
				override def pure[A](it:A):Converter[E,S,A]													= Converter constant it
				override def map[A,B](it:Converter[E,S,A])(func:A=>B):Converter[E,S,B]						= it map func
				override def flatMap[A,B](it:Converter[E,S,A])(func:A=>Converter[E,S,B]):Converter[E,S,B]	= it flatMap func
			}
			
	implicit def ConverterSemigroup[E:Semigroup,S,T]:Semigroup[Converter[E,S,T]]	=
			Semigroup instance (_ orElse _)
}