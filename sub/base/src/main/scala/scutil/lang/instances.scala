package scutil.lang

import scutil.base.implicits._
import scutil.lang.tc._

object instances extends instances

trait instances extends instancesLow {
	//------------------------------------------------------------------------------
	//## builtin
	
	implicit def AutoCloseableResource[T<:AutoCloseable]:Resource[T]	= Resource by (_.close())
	
	implicit def FunctionFunctor[S]:Functor[ ({type l[T]=Function[S,T]})#l ]	=
			new Functor[ ({type l[T]=Function[S,T]})#l ] {
				def map[A,B](it:Function[S,A])(func:A=>B):Function[S,B]	= it andThen func
			}
			
	implicit def PairFunctor[S]:Functor[ ({type l[T]=(S,T)})#l ]	=
			new Functor[ ({type l[T]=(S,T)})#l ] {
				def map[A,B](it:(S,A))(func:A=>B):(S,B)	= (it._1, func(it._2))
			}
			
	// TODO Semigroup[Pair]
			
	implicit def OptionMonad:Monad[Option]	=
			new Monad[Option] {
				override def pure[A](it:A):Option[A]									= Some(it)
				override def map[A,B](it:Option[A])(func:A=>B):Option[B]				= it map func
				override def flatMap[A,B](it:Option[A])(func:A=>Option[B]):Option[B]	= it flatMap func
			}
			
	implicit def OptionMonoid[T]:Monoid[Option[T]]	=
			Monoid by (None, _ orElse _)
		
	implicit def EitherMonad[S]:Monad[ ({type l[T]=Either[S,T]})#l ]	=
			new Monad[ ({type l[T]=Either[S,T]})#l ] {
				override def pure[A](it:A):Either[S,A]										= Right(it)
				override def map[A,B](it:Either[S,A])(func:A=>B):Either[S,B]				= it.right map func
				override def flatMap[A,B](it:Either[S,A])(func:A=>Either[S,B]):Either[S,B]	= it.right flatMap func
			}
			
	implicit def EitherSemigroup[S,T]:Semigroup[Either[S,T]]	=
			Semigroup by { (a,b) =>
				a match {
					case Left(_)	=> b
					case Right(_)	=> a
				}
			}
			
	implicit val StringMonoid:Monoid[String]	=
			Monoid by ("", _ + _)
			
	//------------------------------------------------------------------------------
	//## on function, questionable
	
	implicit def EndoMonoid[T]:Monoid[Endo[T]]	=
			Monoid by (identity, _ andThen _)
		
	implicit def PEndoSemigroup[T]:Semigroup[PEndo[T]]	=
			Semigroup by (_ andThenFixed _)
		
	// TODO do we get this for free with FFunctionFunctor?
	implicit def PFunctionFunctor[S]:Functor[ ({type l[Y]=PFunction[S,Y]})#l ]	=
			new Functor[ ({type l[Y]=PFunction[S,Y]})#l ] {
				def map[A,B](it:PFunction[S,A])(func:A=>B):PFunction[S,B]		= it(_) map func
			}
			
	implicit def PFunctionSemigroup[S,T]:Semigroup[PFunction[S,T]]	=
			Semigroup by (_ orElse _)
			
	implicit def FFunctionFunctor[F[_]:Functor,S]:Functor[ ({type l[Y]=FFunction[F,S,Y]})#l ]	=
			new Functor[ ({type l[Y]=FFunction[F,S,Y]})#l ] {
				def map[A,B](it:FFunction[F,S,A])(func:A=>B):FFunction[F,S,B]	=
						a => (Functor[F] map it(a))(func)
			}
}

trait instancesLow {
	implicit def IdentityMonad:Monad[Identity]	=
			new Monad[Identity] {
				override def pure[A](it:A):Identity[A]										= it
				override def map[A,B](it:Identity[A])(func:A=>B):Identity[B]				= func(it)
				override def flatMap[A,B](it:Identity[A])(func:A=>Identity[B]):Identity[B]	= func(it)
			}
}
