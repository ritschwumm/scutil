package scutil.lang

import scutil.base.implicits._

object Converter {
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
			
	def partial[E,S,T](func:S=>Option[T], bad: =>E):Converter[E,S,T]	=
			Converter { it =>
				Validated goodOr (func(it), bad)
			}
			
	def rejecting[E,T](func:T=>Option[E]):Converter[E,T,T]	=
			Converter { it =>
				Validated badOr (func(it), it)
			}
			
	//------------------------------------------------------------------------------
	
	// TODO boilerplate
	
	def zip2[E:CanConcat,S,T1,T2](d1:Converter[E,S,T1], d2:Converter[E,S,T2]):Converter[E,S,(T1,T2)]	=
			Converter { it =>
				Validated zip2 (d1 apply it, d2 apply it)
			}
				
	def zip3[E:CanConcat,S,T1,T2,T3](d1:Converter[E,S,T1], d2:Converter[E,S,T2], d3:Converter[E,S,T3]):Converter[E,S,(T1,T2,T3)]	=
			Converter { it =>
				Validated zip3 (d1 apply it, d2 apply it, d3 apply it)
			}
			
	def zip4[E:CanConcat,S,T1,T2,T3,T4](d1:Converter[E,S,T1], d2:Converter[E,S,T2], d3:Converter[E,S,T3], d4:Converter[E,S,T4]):Converter[E,S,(T1,T2,T3,T4)]	=
			Converter { it =>
				Validated zip4 (d1 apply it, d2 apply it, d3 apply it, d4 apply it)
			}
			
	def zip5[E:CanConcat,S,T1,T2,T3,T4,T5](d1:Converter[E,S,T1], d2:Converter[E,S,T2], d3:Converter[E,S,T3], d4:Converter[E,S,T4], d5:Converter[E,S,T5]):Converter[E,S,(T1,T2,T3,T4,T5)]	=
			Converter { it =>
				Validated zip5 (d1 apply it, d2 apply it, d3 apply it, d4 apply it, d5 apply it)
			}
			
	//------------------------------------------------------------------------------
	
	def sum[E,S,T](subs:ISeq[PFunction[S,Validated[E,T]]], bad: =>E):Converter[E,S,T]	=
			Converter { it =>
				subs
				.collapseMapFirst	{ _ apply it }
				.getOrElse			(Validated bad bad)
			}
}

// Kleisli[Validated[E,_],S,T]
final case class Converter[+E,-S,+T](apply:S=>Validated[E,T]) {
	def andThen[EE>:E,U](that:Converter[EE,T,U]):Converter[EE,S,U]	=
			Converter { it =>
				this apply it flatMap that.apply
			}
	
	def compose[EE>:E,R](that:Converter[EE,R,S]):Converter[EE,R,T]	=
			that andThen this
		
	def >=>[EE>:E,U](that:Converter[EE,T,U]):Converter[EE,S,U]	=
			this andThen that
	
	def <=<[EE>:E,R](that:Converter[EE,R,S]):Converter[EE,R,T]	=
			this compose that
	
	def map[U](func:T=>U):Converter[E,S,U]		=
			Converter { it =>
				apply(it) map func
			}
			
	def flatMap[EE>:E,SS<:S,U](func:T=>Converter[EE,SS,U]):Converter[EE,SS,U]	=
			Converter { it =>
				this apply it map func flatMap { _ apply it }
			}
		
	def contraMap[R](func:R=>S):Converter[E,R,T]	=
			Converter { it =>
				apply(func(it))
			}
			
	def tag[U](it:U):Converter[E,S,U]	=
			map(constant(it))
		
	def pa[EE>:E:CanConcat,SS<:S,U](that:Converter[EE,SS,T=>U]):Converter[EE,SS,U]	=
			Converter { it =>
				(this apply it) pa (that apply it)
			}
	
	def ap[EE>:E:CanConcat,SS<:S,U,V](that:Converter[EE,SS,U])(implicit ev:T=>U=>V):Converter[EE,SS,V]	=
			Converter { it =>
				(this apply it map ev) ap (that apply it)
			}
	
	def zip[EE>:E,SS<:S,TX](that:Converter[EE,SS,TX])(implicit ev:CanConcat[EE]):Converter[EE,SS,(T,TX)] =
			Converter { it	=>
				(this apply it) zip (that apply it)
			}
			
	def zipWith[EE>:E,SS<:S,TX,U](that:Converter[EE,SS,TX])(func:(T,TX)=>U)(implicit ev:CanConcat[EE]):Converter[EE,SS,U] =
			Converter { it	=>
				((this apply it) zipWith (that apply it))(func)
			}
			
	def coZip[EE>:E,SS,TX>:T](that:Converter[EE,SS,TX]):Converter[EE,Either[S,SS],TX]	=
			Converter {
				case Left(x)	=> this apply x
				case Right(x)	=> that apply x
			}
			
	def orElse[EE>:E:CanConcat,S1<:S,T1>:T](that:Converter[EE,S1,T1]):Converter[EE,S1,T1]	=
			Converter { (it:S1) =>
				(this apply it) orElse (that apply it)
			}
			
	def either[EE>:E,SS,TT](that:Converter[EE,SS,TT]):Converter[EE,Either[S,SS],Either[T,TT]]	=
			Converter {
				case Left(x)	=> this apply x map (Left(_))
				case Right(x)	=> that apply x map (Right(_))
			}
			
	def pair[EE>:E,SS,TT](that:Converter[EE,SS,TT]):Converter[EE,(S,SS),(T,TT)]	=
			Converter { case (s,ss) =>
				this apply s flatMap { t =>
					that apply ss map { tt =>
						(t, tt)
					}
				}
			}
			
	//------------------------------------------------------------------------------
			
	def liftFirst[X]:Converter[E,(S,X),(T,X)]	=
			Converter { case (s,x) =>
				apply(s) map { (_, x) }
			}
			
	def liftSecond[X]:Converter[E,(X,S),(X,T)]	=
			Converter { case (x,s) =>
				apply(s) map { (x, _) }
			}
			
	def liftOption:Converter[E,Option[S],Option[T]]	=
			Converter { it =>
				(it map apply).sequenceValidated
			}
		
	// TODO liftTraversable
			
	def liftISeq[EE>:E](implicit cc:CanConcat[EE]):Converter[EE,ISeq[S],ISeq[T]]	=
			Converter { it =>
				it traverseValidated (apply:S=>Validated[EE,T])
			}
			
	def liftList[EE>:E](implicit cc:CanConcat[EE]):Converter[EE,List[S],List[T]]	=
			Converter { it =>
				it traverseValidated (apply:S=>Validated[EE,T])
			}
			
	def liftVector[EE>:E](implicit cc:CanConcat[EE]):Converter[EE,Vector[S],Vector[T]]	=
			Converter { it =>
				it traverseValidated (apply:S=>Validated[EE,T])
			}
			
	def liftSet[SS<:S,TT>:T,EE>:E](implicit cc:CanConcat[EE]):Converter[EE,Set[SS],Set[TT]]	=
			Converter { it =>
				it traverseValidated (apply:S=>Validated[EE,T])
			}
}
