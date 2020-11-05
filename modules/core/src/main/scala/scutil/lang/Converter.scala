package scutil.lang

import scutil.core.implicits._
import scutil.lang.tc._

object Converter {
	def apply[E,S,T](func:S=>Validated[E,T]):Converter[E,S,T]	=
		new Converter[E,S,T] {
			def convert(s:S):Validated[E,T]	= func(s)
		}

	/** this is useful when building a recursive schema from implicit converters */
	def defer[E,S,T](peer: =>Converter[E,S,T]):Converter[E,S,T]	=
		new Converter[E,S,T] {
			def convert(s:S):Validated[E,T]	= peer convert s
		}

	/** this is useful when building a recursive schema from implicit converters */
	// TODO should this replace the original defer?
	def defer2[E,S,T](peer: =>Converter[E,S,T]):Converter[E,S,T]	=
		new Converter[E,S,T] {
			lazy val cached	= peer
			def convert(s:S):Validated[E,T]	= cached convert s
		}

	def identity[E,T]:Converter[E,T,T]	=
		it => Validated good it

	def constant[E,S,T](it:T):Converter[E,S,T]	= pure(it)

	def pure[E,S,T](it:T):Converter[E,S,T]	=
		_ => Validated good it

	def fail[E,S,T](it:E):Converter[E,S,T]	=
		_ => Validated bad it

	def total[E,S,T](func:S=>T):Converter[E,S,T]	=
		it => Validated good func(it)

	def optional[E,S,T](func:S=>Option[T], bad: =>E):Converter[E,S,T]	=
		it => func(it) toGood bad

	def partial[E,S,T](func:PartialFunction[S,T], bad: =>E):Converter[E,S,T]	=
		optional(func.lift, bad)

	def validate[E,S,T](func:S=>Option[T], bad: S=>E):Converter[E,S,T]	=
		it => func(it) toGood bad(it)

	def rejecting[E,T](func:T=>Option[E]):Converter[E,T,T]	=
		it => func(it) toBad it

	def fromEitherFunction[E,S,T](func:S=>Either[E,T]):Converter[E,S,T]	=
		func(_).toValidated

	def required[E,S,T](base:Converter[E,S,Option[T]], bad: =>E):Converter[E,S,T]	=
		input => base convert input flatMap { _ toGood bad }

	//------------------------------------------------------------------------------

	def sum[E,S,T](subs:Seq[S=>Option[Validated[E,T]]], bad: =>E):Converter[E,S,T]	=
		it => {
			subs
			.collapseMapFirst	{ _ apply it }
			.getOrElse			(Validated bad bad)
		}

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit def ConverterApplicative[E,S](implicit E:Semigroup[E]):Applicative[Converter[E,S,?]]	=
		new Applicative[Converter[E,S,?]] {
			override def pure[A](it:A):Converter[E,S,A]												= Converter pure it
			override def ap[A,B](its:Converter[E,S,A])(func:Converter[E,S,A=>B]):Converter[E,S,B]	= its pa func
		}

	/*
	implicit def ConverterMonad[E,S]:Monad[Converter[E,S,?]]	=
		new Monad[Converter[E,S,?]] {
			override def pure[A](it:A):Converter[E,S,A]													= Converter pure it
			override def map[A,B](it:Converter[E,S,A])(func:A=>B):Converter[E,S,B]						= it map func
			override def flatMap[A,B](it:Converter[E,S,A])(func:A=>Converter[E,S,B]):Converter[E,S,B]	= it flatMap func
		}
	*/

	implicit def ConverterSemigroup[E:Semigroup,S,T]:Semigroup[Converter[E,S,T]]	=
		Semigroup instance (_ orElse _)
}

// Kleisli[Validated[E,_],S,T]
abstract class Converter[E,S,T] {
	@inline final def apply(s:S):Validated[E,T]	= convert(s)

	def convert(s:S):Validated[E,T]

	@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
	def varyIn[SS<:S]:Converter[E,SS,T]		= this.asInstanceOf[Converter[E,SS,T]]
	@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
	def varyOut[TT>:T]:Converter[E,S,TT]	= this.asInstanceOf[Converter[E,S,TT]]
	@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
	def varyError[EE>:E]:Converter[EE,S,T]	= this.asInstanceOf[Converter[EE,S,T]]

	def asFunction:S=>Validated[E,T]	= convert _

	//------------------------------------------------------------------------------

	def andThen[U](that:Converter[E,T,U]):Converter[E,S,U]	=
		it => this convert it flatMap that.convert

	def compose[R](that:Converter[E,R,S]):Converter[E,R,T]	=
		that andThen this

	def >=>[U](that:Converter[E,T,U]):Converter[E,S,U]	=
		this andThen that

	def <=<[R](that:Converter[E,R,S]):Converter[E,R,T]	=
		this compose that

	def map[U](func:T=>U):Converter[E,S,U]		=
		it => convert(it) map func

	def flatMap[U](func:T=>Converter[E,S,U]):Converter[E,S,U]	=
		it => this convert it flatMap { jt => func(jt) convert it }

	def contraMap[R](func:R=>S):Converter[E,R,T]	=
		it => convert(func(it))

	def as[U](it:U):Converter[E,S,U]	=
		map(constant(it))

	/** function effect first */
	def pa[U](that:Converter[E,S,T=>U])(implicit cc:Semigroup[E]):Converter[E,S,U]	=
		it => (this convert it) pa (that convert it)

	/** function effect first */
	def ap[U,V](that:Converter[E,S,U])(implicit ev:T=>U=>V, cc:Semigroup[E]):Converter[E,S,V]	=
		that pa (this map ev)

	def tuple[U](that:Converter[E,S,U])(implicit cc:Semigroup[E]):Converter[E,S,(T,U)] =
		it	=> (this convert it) tuple (that convert it)

	def map2[U,V](that:Converter[E,S,U])(func:(T,U)=>V)(implicit cc:Semigroup[E]):Converter[E,S,V] =
		it	=> ((this convert it) map2 (that convert it))(func)

	def coZip[SS](that:Converter[E,SS,T]):Converter[E,Either[S,SS],T]	=
		{
			case Left(x)	=> this convert x
			case Right(x)	=> that convert x
		}

	def orElse(that:Converter[E,S,T])(implicit cc:Semigroup[E]):Converter[E,S,T]	=
		it => (this convert it) orElse (that convert it)

	def either[SS,TT](that:Converter[E,SS,TT]):Converter[E,Either[S,SS],Either[T,TT]]	=
		{
			case Left(x)	=> this convert x map (Left(_))
			case Right(x)	=> that convert x map (Right(_))
		}

	def pair[SS,TT](that:Converter[E,SS,TT])(implicit E:Semigroup[E]):Converter[E,(S,SS),(T,TT)]	=
		{ case (s,ss) => (this convert s) tuple (that convert ss) }

	//------------------------------------------------------------------------------

	def liftFirst[X]:Converter[E,(S,X),(T,X)]	= { case (s,x) => convert(s) map { (_, x) } }
	def liftSecond[X]:Converter[E,(X,S),(X,T)]	= { case (x,s) => convert(s) map { (x, _) } }

	//------------------------------------------------------------------------------

	// NOTE this uses the Traverse instance instead of a native implementation
	def liftTraversed[F[_]](implicit F:Traversed[F], CC:Semigroup[E]):Converter[E,F[S],F[T]]	= _ traverse convert

	def liftOption:Converter[E,Option[S],Option[T]]								= _ traverseValidated convert
	def liftSeq(implicit cc:Semigroup[E]):Converter[E,Seq[S],Seq[T]]			= _ traverseValidated convert
	def liftList(implicit cc:Semigroup[E]):Converter[E,List[S],List[T]]			= _ traverseValidated convert
	def liftVector(implicit cc:Semigroup[E]):Converter[E,Vector[S],Vector[T]]	= _ traverseValidated convert
	def liftSet(implicit cc:Semigroup[E]):Converter[E,Set[S],Set[T]]			= _ traverseValidated convert

	// NOTE this uses the Traverse instance instead of a native implementation
	def liftNes(implicit cc:Semigroup[E]):Converter[E,Nes[S],Nes[T]]			= _ traverse convert

	//------------------------------------------------------------------------------

	def toEitherFunction:S=>Either[E,T]	=
		convert _ andThen (_.toEither)
}
