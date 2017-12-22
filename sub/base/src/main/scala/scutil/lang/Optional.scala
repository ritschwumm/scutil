package scutil.lang

import scutil.lang.implicits._
import scutil.lang.tc._

object Optional extends OptionalInstances {
	def partial[S,T](get:PartialFunction[S,T], put:T=>S):Optional[S,T] =
			Optional(get.lift, (s,t) => put(t))
		
	def total[S,T](get:S=>T, put:T=>S):Optional[S,T]	=
			Optional(
				get	= get andThen Some.apply,
				put	= (s, t)	=> put(t)
			)
			
	def identity[T]:Optional[T,T]	=
			Optional total (Predef.identity, Predef.identity)
		
	def trivial[T]:Optional[T,Unit]	=
			Optional(
				get	= t 	=> Some(()),
				put	= (t,x) => t
			)
			
	def always[T]:Optional[Option[T],T]	=
			Optional(Predef.identity, (t,x) => Some(x))
		
	def filtered[T](pred:T=>Boolean):Optional[T,T]	=
			Optional(
				it		=> if (pred(it)) Some(it) else None,
				(t,x)	=> x
			)
			
	def codiag[T]:Optional[Either[T,T],T]	=
			identity[T] sum identity[T]
}

final case class Optional[S,T](
	get:S=>Option[T],
	put:(S,T)=>S
) {
	def mod(func:Endo[T]):Endo[S]	=
			s	=> {
				get(s)
				.map		{ t => put(s, func(t)) }
				.getOrElse	(s)
			}
	def modThe(s:S, func:Endo[T]):S	= mod(func) apply s
			
	def modF[F[_]](func:FEndo[F,T])(implicit F:Applicative[F]):FEndo[F,S]	=
			s	=> modOptF(func) apply s getOrElse (F pure s)
	def modTheF[F[_]](s:S, func:FEndo[F,T])(implicit F:Applicative[F]):F[S]	= modF(func) apply s
	
	//------------------------------------------------------------------------------
	
	def modOpt(func:Endo[T]):PEndo[S]	=
			s	=> {
				get(s) map { t =>
					put(s, func(t))
				}
			}
	def modTheOpt(s:S, func:Endo[T]):Option[S]	= modOpt(func) apply s
	
	def modOptF[F[_]](func:FEndo[F,T])(implicit F:Functor[F]):S=>Option[F[S]]	=
			s	=> {
				get(s) map { t =>
					val ft:F[T] = func(t)
					(F map ft) { t => put(s, t) }
				}
			}
	def modTheOptF[F[_]](s:S, func:FEndo[F,T])(implicit F:Functor[F]):Option[F[S]]	= modOptF(func) apply s
		
		
	// TODO use this with State and StateT
	
	//------------------------------------------------------------------------------
	
	def orElse(that:Optional[S,T]):Optional[S,T]	=
			Optional(
				get	= s	=> (this get s) orElse (that get s),
				put	= put
			)
			
	/** filter the source value */
	def filterBefore(pred:Predicate[S]):Optional[S,T]	=
			Optional(
				s	=> if (pred(s)) get(s) else None,
				put
			)
			
	/** filter the target value */
	def filterAfter(pred:Predicate[T]):Optional[S,T]	=
			Optional(
				s	=> get(s) filter pred,
				put
			)
	
	//------------------------------------------------------------------------------
	
	/** symbolic alias for andThen */
	def >=>[U](that:Optional[T,U]):Optional[S,U]	=
			this andThen that
		
	/** symbolic alias for compose */
	def <=<[R](that:Optional[R,S]):Optional[R,T]	=
			this compose that
		
	def compose[R](that:Optional[R,S]):Optional[R,T]	=
			that andThen this
		
	def andThen[U](that:Optional[T,U]):Optional[S,U]	=
			Optional(
				get	= s => this get s flatMap that.get,
				put	= (s:S, u:U) => {
					get(s)
					.map { t =>
						this put (s, that put (t, u))
					}
					.getOrElse(s)
				}
			)
	
	//------------------------------------------------------------------------------
	
	// TODO optics does this mean we have an Applicative instance?
	def zip[U](that:Optional[S,U]):Optional[S,(T,U)]	=
			Optional(
				get	= s => (this get s) zip (that get s),
				put	= (s, tu) => that put (this put (s, tu._1), tu._2)
			)
			
	def sum[SS](that:Optional[SS,T]):Optional[Either[S,SS],T]	=
			Optional(
				get	= s =>
					s match {
						case Left(s)	=> this get s
						case Right(ss)	=> that get ss
					},
				put	= (sss, t) =>
					sss match {
						case Left(s)	=> Left(this put (s, t))
						case Right(ss)	=> Right(that put (ss, t))
					}
			)
			
	def product[SS,TT](that:Optional[SS,TT]):Optional[(S,SS),(T,TT)]	=
			Optional(
				get	= sss => (this get sss._1) zip (that get sss._2),
				put	= (sss, ttt) => (this put (sss._1, ttt._1), that put (sss._2, ttt._2))
			)
}

trait OptionalInstances {
	// TODO optics is this lawful?
	implicit def OptionalSemigroup[S,T]:Semigroup[Optional[S,T]]	=
			Semigroup instance (_ orElse _)
}

