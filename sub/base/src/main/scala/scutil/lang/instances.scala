package scutil.lang

import scutil.base.implicits._
import scutil.lang.tc._

object instances extends instances

trait instances extends instancesLow {
	//------------------------------------------------------------------------------
	//## builtin
	
	implicit def AutoCloseableResource[T<:AutoCloseable]:Resource[T]	= Resource instance (_.close())
	
	implicit def FunctionFunctor[S]:Functor[Function[S,?]]	=
			new Functor[Function[S,?]] {
				def map[A,B](it:Function[S,A])(func:A=>B):Function[S,B]	= it andThen func
			}
			
	implicit def PairFunctor[S]:Functor[(S,?)]	=
			new Functor[(S,?)] {
				def map[A,B](it:(S,A))(func:A=>B):(S,B)	= (it._1, func(it._2))
			}
			
	// TODO Semigroup[Pair]
			
	implicit def OptionTraversedMonad:TraversedMonad[Option]	=
			new TraversedMonad[Option] {
				override def map[A,B](it:Option[A])(func:A=>B):Option[B]				= it map func

				override def pure[A](it:A):Option[A]									= Some(it)
				override def flatMap[A,B](it:Option[A])(func:A=>Option[B]):Option[B]	= it flatMap func
				
				override def traverse[G[_],S,T](it:Option[S])(func:S=>G[T])(implicit AP:Applicative[G]):G[Option[T]]	=
						it match {
							case None		=> AP pure None
							case Some(s)	=> (AP map func(s))(Some.apply)
						}
			}
			
	implicit def OptionMonoid[T](implicit S:Semigroup[T]):Monoid[Option[T]]	=
			Monoid instance (
				None,
				(a,b) => (a,b) match {
					case (None,		None)		=> None
					case (Some(aa),	None)		=> Some(aa)
					case (None,		Some(bb))	=> Some(bb)
					case (Some(aa),	Some(bb))	=> Some(S concat (aa, bb))
				}
			)
		
	implicit def EitherTraversedMonad[S]:TraversedMonad[Either[S,?]]	=
			new TraversedMonad[Either[S,?]] {
				override def pure[A](it:A):Either[S,A]										= Right(it)
				override def map[A,B](it:Either[S,A])(func:A=>B):Either[S,B]				= it.right map func
				override def flatMap[A,B](it:Either[S,A])(func:A=>Either[S,B]):Either[S,B]	= it.right flatMap func
				
				override def traverse[G[_],A,B](it:Either[S,A])(func:A=>G[B])(implicit AP:Applicative[G]):G[Either[S,B]]	=
						it match {
							case Left(x)	=> AP pure Left(x)
							case Right(x)	=> (AP map func(x))(Right.apply)
						}
			}
			
	implicit def EitherSemigroup[S,T]:Semigroup[Either[S,T]]	=
			Semigroup instance { (a,b) =>
				a match {
					case Left(_)	=> b
					case Right(_)	=> a
				}
			}
			
	implicit val StringMonoid:Monoid[String]	=
			Monoid instance ("", _ + _)
		
	implicit val StringShow:Show[String]	=
			Show instance identity
		
	implicit val ByteShow:Show[Byte]	=
			Show.toStringInstance
			
	implicit val ShortShow:Show[Short]	=
			Show.toStringInstance
			
	implicit val IntShow:Show[Int]	=
			Show.toStringInstance
			
	implicit val LongShow:Show[Long]	=
			Show.toStringInstance
			
	implicit val FloatShow:Show[Float]	=
			Show.toStringInstance
			
	implicit val DoubleShow:Show[Double]	=
			Show.toStringInstance
			
	implicit val CharShow:Show[Char]	=
			Show.toStringInstance
			
	implicit val BooleanShow:Show[Boolean]	=
			Show.toStringInstance
		
	implicit val BigDecimalShow:Show[BigDecimal]	=
			Show.toStringInstance
			
	//------------------------------------------------------------------------------
	//## on function, questionable
	
	implicit def EndoMonoid[T]:Monoid[Endo[T]]	=
			Monoid instance (identity, _ andThen _)
		
	implicit def PEndoSemigroup[T]:Semigroup[PEndo[T]]	=
			Semigroup instance (_ andThenFixed _)
		
	// TODO do we get this for free with FFunctionFunctor?
	// NOTE PFunction and FFunction are both just ReaderT/Kleisli
	implicit def PFunctionFunctor[S]:Functor[PFunction[S,?]]	=
			new Functor[PFunction[S,?]] {
				def map[A,B](it:PFunction[S,A])(func:A=>B):PFunction[S,B]		= it(_) map func
			}
			
	implicit def PFunctionSemigroup[S,T]:Semigroup[PFunction[S,T]]	=
			Semigroup instance (_ orElse _)
			
	implicit def FFunctionFunctor[F[_]:Functor,S]:Functor[FFunction[F,S,?]]	=
			new Functor[FFunction[F,S,?]] {
				def map[A,B](it:FFunction[F,S,A])(func:A=>B):FFunction[F,S,B]	=
						a => (Functor[F] map it(a))(func)
			}
}

trait instancesLow {
	implicit def IdentityTraversedMonad:TraversedMonad[Identity]	=
			new TraversedMonad[Identity] {
				override def map[A,B](it:Identity[A])(func:A=>B):Identity[B]				= func(it)
				override def pure[A](it:A):Identity[A]										= it
				override def flatMap[A,B](it:Identity[A])(func:A=>Identity[B]):Identity[B]	= func(it)
				override def traverse[G[_],S,T](it:Identity[S])(func:S=>G[T])(implicit AP:Applicative[G]):G[Identity[T]]	= func(it)
			}
}
