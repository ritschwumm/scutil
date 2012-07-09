package scutil.data

object Tried {
	def win[F,W](it:W):Tried[F,W]	= Win(it)
	def fail[F,W](it:F):Tried[F,W]	= Fail(it)
}

/** right biased Either (with swapped type parameters), Try with parameterized error */
sealed trait Tried[+F,+W] {
	def cata[X](fail:F=>X, win:W=>X):X	=
			this match {
				case Win(x)		=> win(x)
				case Fail(x)	=> fail(x)
			}
			
	def isWin:Boolean	= 
			cata(_ => true,  _ => false)
		
	def isFail:Boolean	=
			cata(_ => false, _ => true)
	
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
	
	def orElse[FF>:F,WW>:W](that: =>Tried[FF,WW]):Tried[FF,WW]	= 
			cata(_ => that, Win.apply)
		
	def getOrElse[WW>:W](that: =>WW):WW	= 
			cata(_ => that, identity)
		
	def getOrError(s:String):W	=
			getOrElse(sys error s) 
		
	//------------------------------------------------------------------------------
	
	def filter[FF>:F](fail: =>FF)(func:W=>Boolean):Tried[FF,W]	=
			cata(Fail.apply, it => if (func(it)) Win(it) else Fail(fail))
			
	def filterMap[FF>:F,X](fail: =>FF)(func:W=>Option[X]):Tried[FF,X]	=
			cata(Fail.apply, it => func(it) map Win.apply getOrElse Fail(fail))
		
	def collect[FF>:F,X](fail: =>FF)(func:PartialFunction[W,X]):Tried[FF,X]	=
			filterMap(fail)(func.lift)
		
	// def mapOr[WW>:W,X](win: =>WW, func:F=>Option[X]):Tried[X,WW]	=
	// 		// swap.filterMap(win)(func).swap
	// 		cata(it => func(it) map Fail.apply getOrElse Win(win), Win.apply)
			
	def rescue[WW>:W](func:F=>Option[WW]):Tried[F,WW]	=
			cata(it => func(it) map Win.apply getOrElse Fail(it), Win.apply)
		
	def reject[FF>:F](func:W=>Option[FF]):Tried[FF,W]	=
			// swap.rescue(func).swap
			cata(Fail.apply, it => func(it) map Fail.apply getOrElse Win(it))
		
	//------------------------------------------------------------------------------
		
	def toEither:Either[F,W]	= 
			cata(Left.apply, Right.apply)
		
	def toOption:Option[W]	= 
			cata(_ => None, Some.apply)
		
	def toList:List[W]	= 
			cata(_ => Nil, List(_))
}

case class Win[F,W](value:W)	extends Tried[F,W]
case class Fail[F,W](value:F)	extends Tried[F,W]
