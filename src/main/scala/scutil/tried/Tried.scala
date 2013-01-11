package scutil.tried

object Tried {
	def win[F,W](it:W):Tried[F,W]	= Win(it)
	def fail[F,W](it:F):Tried[F,W]	= Fail(it)
	
	def optional[F,W](value:Option[W], default: =>F):Tried[F,W]	=
			value match {
				case Some(win)	=> Win(win)
				case None		=> Fail(default)
			}
			
	def notNull[T](value:T):Tried[Null,T]	= 
			if (value != null)	Win(value)
			else				Fail(null)
	
	def allCatch[T](value: =>T):Tried[Throwable,T]	=
			try { Win(value) }
			catch { case e => Fail(e) }
}

/** right biased Either (with swapped type parameters), Try with parameterized error */
sealed trait Tried[+F,+W] {
	def cata[X](fail:F=>X, win:W=>X):X	=
			this match {
				case Win(x)		=> win(x)
				case Fail(x)	=> fail(x)
			}
			
	/** same as cata(identity,identity) but with improved type inference */
	def upfold[U](implicit ev:this.type <:< Tried[U,U]):U	=
			ev(this) cata (identity, identity)
		
	//------------------------------------------------------------------------------
	
	def isWin:Boolean	= 
			cata(_ => false,  _ => true)
		
	def isFail:Boolean	=
			cata(_ => true, _ => false)
		
	//------------------------------------------------------------------------------
		
	def exists(func:W=>Boolean):Boolean	=
			cata(_ => false, func)
		
	def forall(func:W=>Boolean):Boolean	=
			cata(_ => true, func)
		
	//------------------------------------------------------------------------------
	
	def iterator:Iterator[W] =
			cata(_ => Iterator.empty, Iterator.single)
	
	def foreach(func:W=>Unit):Unit	= 
			cata(_ => (), func)
		
	def map[X](func:W=>X):Tried[F,X]	= 
			cata(Fail.apply, func andThen Win.apply)
		
	def flatMap[FF>:F,X](func:W=>Tried[FF,X]):Tried[FF,X]	= 
			cata(Fail.apply, func)
		
	def flatten[FF>:F,X](implicit ev: W=>Tried[FF,X]):Tried[FF,X]	=
			flatMap(ev)
	
	/** fail on this overrides fail on that */
	def pa[FF>:F,X](that:Tried[FF,W=>X]):Tried[FF,X]	= 
			cata(
				v	=> that cata (
					f	=> Fail(f),
					f	=> Fail(v)
				),
				v	=> that cata (
					f	=> Fail(f),
					f	=> Win(f(v))
				)
			)
			
	/** fail on this overrides fail on that */
	def ap[FF>:F,X,Y](that:Tried[FF,X])(implicit ev: W=>X=>Y):Tried[FF,Y]	=
			cata(
				f	=> that cata (
					v	=> Fail(v),
					v	=> Fail(f)
				),
				f	=> that cata (
					v	=> Fail(v),
					v	=> Win(f(v))
				)
			)
		
	//------------------------------------------------------------------------------
			
	def swap:Tried[W,F]	= 
			cata(Win.apply,	Fail.apply)
		
	def withSwapped[FX,WX](func:Tried[W,F]=>Tried[WX,FX]):Tried[FX,WX]	=
			func(swap).swap
	
	//------------------------------------------------------------------------------
	
	def getOrElse[WW>:W](that: =>WW):WW	= 
			cata(_ => that, identity)
		
	def getOrError(s:String):W	=
			getOrElse(sys error s) 
		
	def orElse[FF>:F,WW>:W](that: =>Tried[FF,WW]):Tried[FF,WW]	= 
			cata(_ => that, Win.apply)
		
	//------------------------------------------------------------------------------
	
	def rescue[WW>:W](func:F=>Option[WW]):Tried[F,WW]	=
			cata(it => func(it) map Win.apply getOrElse Fail(it), Win.apply)
		
	def reject[FF>:F](func:W=>Option[FF]):Tried[FF,W]	=
			cata(Fail.apply, it => func(it) map Fail.apply getOrElse Win(it))
		
	def filterOr[FF>:F](func:W=>Boolean, fail:FF):Tried[FF,W]	=
			cata(Fail.apply, it => if (func(it)) Win(it) else Fail(fail))
			
	def mapOr[FF>:F,WW](func:W=>Option[WW], fail:FF):Tried[FF,WW]	=
			cata(Fail.apply, it => func(it) map Win.apply getOrElse Fail(fail))
		
	def collectOr[FF>:F,WW](func:PartialFunction[W,WW], fail:FF):Tried[FF,WW]	=
			cata(Fail.apply, it => if (func isDefinedAt it) Win(func(it)) else Fail(fail))
 	 
	//------------------------------------------------------------------------------
		
	def toEither:Either[F,W]	= 
			cata(Left.apply, Right.apply)
		
	def toOption:Option[W]	= 
			cata(_ => None, Some.apply)
		
	def toSeq:Seq[W]	= 
			toList
		
	def toList:List[W]	= 
			cata(_ => Nil, List(_))
		
	def toVector:Vector[W]	= 
			cata(_ => Vector.empty, Vector(_))
}

case class Win[F,W](value:W)	extends Tried[F,W]
case class Fail[F,W](value:F)	extends Tried[F,W]
