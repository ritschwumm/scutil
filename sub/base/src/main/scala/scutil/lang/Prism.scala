package scutil.lang

import scutil.lang.implicits._
import scutil.lang.tc._

object Prism extends PrismInstances {
	def partial[S,T](get:PartialFunction[S,T], put:T=>S):Prism[S,T] =
			Prism(get.lift, put)
	
	def total[S,T](get:S=>T, put:T=>S):Prism[S,T] =
			Prism(get andThen Some.apply, put)
			
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
final case class Prism[S,T](get:PFunction[S,T], put:T=>S) {
	// can be used as scala function and extractor
	def apply(t:T):S			= put(t)
	def unapply(s:S):Option[T]	= get(s)
	
	//------------------------------------------------------------------------------
	
	// these fall back to the original value if necessary
	
	def mod(func:Endo[T]):Endo[S]	= s => get(s) map (func andThen put) getOrElse s
	def modThe(s:S, func:Endo[T]):S	= mod(func)(s)
	
	def modF[F[_]](func:FEndo[F,T])(implicit F:Applicative[F]):FEndo[F,S]	=
			s	=> modOptF(func) apply s getOrElse (F pure s)
	def modTheF[F[_]](s:S, func:FEndo[F,T])(implicit F:Applicative[F]):F[S]	= modF(func) apply s
	
	//------------------------------------------------------------------------------
	
	def modOpt(func:Endo[T]):PEndo[S]			= s => get(s) map (func andThen put)
	def modTheOpt(s:S, func:Endo[T]):Option[S]	= modOpt(func) apply s
	
	def modOptF[F[_]](func:FEndo[F,T])(implicit F:Functor[F]):S=>Option[F[S]]	=
			s	=> {
				get(s) map { t =>
					(F map func(t)) { ss =>
						put(ss)
					}
				}
			}
	def modTheOptF[F[_]](s:S, func:FEndo[F,T])(implicit F:Functor[F]):Option[F[S]]	=
			modOptF(func) apply s
	
	//------------------------------------------------------------------------------
	
	def embedState[U](state:State[T,U]):State[S,Option[U]]	=
			State { s =>
				(get(s) map state.run)
				.map		{ tu => put(tu._1) -> (Some(tu._2):Option[U]) }
				.getOrElse	(s -> None)
			}
			
	def embedStateT[F[_],U](state:StateT[F,T,U])(implicit F:Applicative[F]):StateT[F,S,Option[U]]	=
			StateT { s =>
				(get(s) map state.run)
				.map { ftu =>
					(F map ftu) { tu =>
						put(tu._1) -> (Some(tu._2):Option[U])
					}
				}
				.getOrElse (
					F pure ((s -> (None:Option[U])))
				)
			}
			
	def embedStateOpt[U](state:State[T,U]):StateT[Option,S,U]	=
			StateT { (s:S) =>
				val otu:Option[(T,U)]	= get(s) map state.run
				otu map { tu => (put(tu._1), tu._2) }
			}
			
	//------------------------------------------------------------------------------
	
	def orElse(that:Prism[S,T]):Prism[S,T]	=
			Prism(
				get	= s	=> (this get s) orElse (that get s),
				put
			)
					
	/** filter the source value */
	def filterBefore(pred:Predicate[S]):Prism[S,T]	=
			Prism(
				get	= s	=> if (pred(s)) get(s) else None,
				put
			)
			
	/** filter the target value */
	def filterAfter(pred:Predicate[T]):Prism[S,T]	=
			Prism(
				get	= s	=> get(s) filter pred,
				put
			)
					
	//------------------------------------------------------------------------------
			
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
				get	= s	=> this get s flatMap that.get,
				put	= t	=> this put (that put t)
			)
			
	//------------------------------------------------------------------------------
		
	// impossible
	// def zip[U](that:Prism[S,U]):Prism[S,(T,U)]
	// def sum[SS](that:Prism[SS,T]):Prism[Either[S,SS],T]
			
	// ***
	def product[SS,TT](that:Prism[SS,TT]):Prism[(S,SS),(T,TT)]	=
			Prism(
				get	= sss 	=> (this get sss._1) zip (that get sss._2),
				put	= ttt	=> (this put ttt._1, that put ttt._2)
			)
			
	//------------------------------------------------------------------------------
		
	def toPBijection:PBijection[S,T]	=
			PBijection(
				get	= get,
				put	= t => Some(put(t))
			)
			
	def toOptional:Optional[S,T]	=
			Optional(
				get	= get,
				put	= (s, t)	=> put(t)
			)
					
	def toPLens:PLens[S,T]	=
			PLens {
				this get _ map (Store(_, this.put))
			}
		
	def writeExtractor:Extractor[S,T]	=
			Extractor(get)
			
	// TODO optics this is questionable
	def toBijection(func:S=>T):Bijection[S,T]	=
			Bijection(
				get	= s => get(s) getOrElse func(s),
				put	= put
			)
					
	// TODO optics this is questionable
	def toBijectionWith(default: =>T):Bijection[S,T]	=
			toBijection(constant(default))
}

trait PrismInstances {
	// TODO optics is this lawful?
	implicit def PrismSemigroup[S,T]:Semigroup[Prism[S,T]]	=
			Semigroup instance (_ orElse _)
}