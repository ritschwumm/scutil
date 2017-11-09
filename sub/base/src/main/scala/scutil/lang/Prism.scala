package scutil.lang

import scutil.lang.tc._

object Prism extends PrismInstances {
	def partial[S,T](writeFunc:PartialFunction[S,T], readFunc:T=>S):Prism[S,T] =
			Prism(writeFunc.lift, readFunc)
	
	def total[S,T](writeFunc:S=>T, readFunc:T=>S):Prism[S,T] =
			Prism(writeFunc andThen Some.apply, readFunc)
			
	def identity[T]:Prism[T,T] =
			total[T,T](Predef.identity[T], Predef.identity[T])
		
	def always[T]:Prism[Option[T],T]	=
			Prism(Predef.identity, Some.apply)
			
	def filtered[T](pred:T=>Boolean):Prism[T,T]	=
			Prism(
				it => if (pred(it)) Some(it) else None,
				Predef.identity
			)
}

/** parser and unparser for some data into a side format, aka Prism' */
final case class Prism[S,T](write:PFunction[S,T], read:T=>S) {
	// can be used as scala function and extractor
	def apply(t:T):S			= read(t)
	def unapply(s:S):Option[T]	= write(s)
	
	def get(s:S):Option[T]		= write(s)
	def getter:PFunction[S,T]	= write
	
	def modify(s:S, func:Endo[T]):Option[S]	= write(s) map (func andThen read)
	def modifier(func:Endo[T]):PEndo[S]		= modify(_, func)
	
	def modifyF[F[_]](s:S, func:FEndo[F,T])(implicit F:Functor[F]):Option[F[S]]	= {
		write(s) map { t =>
			(F map func(t)) { ss =>
				read(ss)
			}
		}
	}
	def modifierF[F[_]](func:FEndo[F,T])(implicit F:Functor[F]):S=>Option[F[S]]	= modifyF(_, func)
	
	def modifyState[X](s:S, func:State[T,X]):Option[(S,X)]	=
			write(s) map func.run map { case (t, x) =>
				(read(t), x)
			}
			
	def modifierState[X](func:State[T,X]):S=>Option[(S,X)]	= modifyState(_, func)
	
	def modifyStateT[F[_],X](s:S, func:StateT[F,T,X])(implicit F:Functor[F]):Option[F[(S,X)]]	=
			write(s) map func.run map { (a:F[(T,X)]) =>
				(F map a) { case (t, x) =>
					(read(t), x)
				}
			}
	
	def modifierStateT[F[_],X](func:StateT[F,T,X])(implicit F:Functor[F]):S=>Option[F[(S,X)]]	= modifyStateT(_, func)
	
	//------------------------------------------------------------------------------
	
	def orElse(that:Prism[S,T]):Prism[S,T]	=
			Prism(
				s	=> (this write s) orElse (that write s),
				read
			)
					
	/** filter the source value */
	def filterBefore(pred:Predicate[S]):Prism[S,T]	=
			Prism(
				s	=> if (pred(s)) write(s) else None,
				read
			)
			
	/** filter the target value */
	def filterAfter(pred:Predicate[T]):Prism[S,T]	=
			Prism(
				s	=> write(s) filter pred,
				read
			)
					
	/** symbolic alias for andThen */
	def >=>[U](that:Prism[T,U]):Prism[S,U]	=
			this andThen that
		
	/** symbolic alias for compose */
	def <=<[R](that:Prism[R,S]):Prism[R,T]	=
			this compose that
		
	def compose[R](that:Prism[R,S]):Prism[R,T]	=
			that andThen this
			
	def andThen[U](that:Prism[T,U]):Prism[S,U]	=
			Prism(
				s	=> this write s flatMap that.write,
				t	=> this read (that read t)
			)
					
	def andThenBijection[U](that:Bijection[T,U]):Prism[S,U]	=
			this >=> that.toPrism
					
	def andThenPBijection[U](that:PBijection[T,U]):PBijection[S,U]	=
			toPBijection >=> that
					
	def andThenPLens[U](that:PLens[T,U]):PLens[S,U]	=
			toPLens >=> that
			
	def toPBijection:PBijection[S,T]	=
			PBijection(
				write,
				t => Some(read(t))
			)
					
	def toPLens:PLens[S,T]	=
			PLens {
				this write _ map (Store(_, this.read))
			}
		
	def writeExtractor:Extractor[S,T]	=
			Extractor(write)
			
	def toBijection(func:S=>T):Bijection[S,T]	=
			Bijection(
				s => write(s) getOrElse func(s),
				read
			)
					
	def toBijectionWith(default: =>T):Bijection[S,T]	=
			toBijection(constant(default))
}

trait PrismInstances {
	implicit def PrismSemigroup[S,T]:Semigroup[Prism[S,T]]	=
			Semigroup instance (_ orElse _)
}