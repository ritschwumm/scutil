package scutil.lang.tc

import scutil.lang._

trait CanMapInstances extends CanMapInstancesLow {
	//------------------------------------------------------------------------------
	//## builtin
	
	implicit def FunctionCanMap[S]:CanMap[ ({type l[T]=Function[S,T]})#l ]	=
			new CanMap[ ({type l[T]=Function[S,T]})#l ] {
				def map[A,B](it:Function[S,A])(func:A=>B):Function[S,B]	= it andThen func
			}
			
	implicit def PairCanMap[S]:CanMap[ ({type l[T]=(S,T)})#l ]	=
			new CanMap[ ({type l[T]=(S,T)})#l ] {
				def map[A,B](it:(S,A))(func:A=>B):(S,B)				= (it._1, func(it._2))
			}
			
	implicit def EitherCanMap[S]:CanMap[ ({type l[T]=Either[S,T]})#l ]	=
			new CanMap[ ({type l[T]=Either[S,T]})#l ] {
				def map[A,B](it:Either[S,A])(func:A=>B):Either[S,B]	= it.right map func
			}
			
	implicit def OptionCanMap:CanMap[Option]	=
			new CanMap[Option] {
				def map[A,B](it:Option[A])(func:A=>B):Option[B]		= it map func
			}
			
	implicit def VectorCanMap:CanMap[Vector]	=
			new CanMap[Vector] {
				def map[A,B](it:Vector[A])(func:A=>B):Vector[B]		= it map func
			}
			
	implicit def ListCanMap:CanMap[List]	=
			new CanMap[List] {
				def map[A,B](it:List[A])(func:A=>B):List[B]			= it map func
			}
			
	//------------------------------------------------------------------------------
	//## own
			
	implicit def ConverterCanMap[E,S]:CanMap[ ({type l[T]=Converter[E,S,T]})#l ]	=
			new CanMap[ ({type l[T]=Converter[E,S,T]})#l ] {
				def map[A,B](it:Converter[E,S,A])(func:A=>B):Converter[E,S,B]	= it map func
			}
			
	implicit def ExtractorCanMap[S]:CanMap[ ({type l[T]=Extractor[S,T]})#l ]	=
			new CanMap[ ({type l[T]=Extractor[S,T]})#l ] {
				def map[A,B](it:Extractor[S,A])(func:A=>B):Extractor[S,B]		= it map func
			}
			
	implicit def NesCanMap:CanMap[Nes]	=
			new CanMap[Nes] {
				def map[A,B](it:Nes[A])(func:A=>B):Nes[B]						= it map func
			}
			
	implicit def StoreCanMap[S]:CanMap[ ({type l[T]=Store[T,S]})#l ]	=
			new CanMap[ ({type l[T]=Store[T,S]})#l ] {
				def map[A,B](it:Store[A,S])(func:A=>B):Store[B,S]				= it map func
			}
			
	implicit def TriedCanMap[S]:CanMap[ ({type l[T]=Tried[S,T]})#l ]	=
			new CanMap[ ({type l[T]=Tried[S,T]})#l ] {
				def map[A,B](it:Tried[S,A])(func:A=>B):Tried[S,B]				= it map func
			}
			
	implicit def ValidatedCanMap[S]:CanMap[ ({type l[T]=Validated[S,T]})#l ]	=
			new CanMap[ ({type l[T]=Validated[S,T]})#l ] {
				def map[A,B](it:Validated[S,A])(func:A=>B):Validated[S,B]		= it map func
			}
			
	// TODO do we get this for free with FFunctionCanMap?
	implicit def PFunctionCanMap[S]:CanMap[ ({type l[Y]=PFunction[S,Y]})#l ]	=
			new CanMap[ ({type l[Y]=PFunction[S,Y]})#l ] {
				def map[A,B](it:PFunction[S,A])(func:A=>B):PFunction[S,B]		= it(_) map func
			}
			
	implicit def FFunctionCanMap[F[_]:CanMap,S]:CanMap[ ({type l[Y]=FFunction[F,S,Y]})#l ]	=
			new CanMap[ ({type l[Y]=FFunction[F,S,Y]})#l ] {
				def map[A,B](it:FFunction[F,S,A])(func:A=>B):FFunction[F,S,B]	=
						a => (CanMap[F] map it(a))(func)
			}
			
	implicit def StatefulCanMap[T,X]:CanMap[ ({type l[Y]=Stateful[T,Y]})#l ]	=
			new CanMap[ ({type l[Y]=Stateful[T,Y]})#l ] {
				def map[A,B](it:Stateful[T,A])(func:A=>B):Stateful[T,B]	=
						it(_) match { case (t,a) => (t,func(a)) }
			}
			
	// TODO do we get this for free with FStatefulCanMap?
	implicit def PStatefulCanMap[T,X]:CanMap[ ({type l[Y]=PStateful[T,Y]})#l ]	=
			new CanMap[ ({type l[Y]=PStateful[T,Y]})#l ] {
				def map[A,B](it:PStateful[T,A])(func:A=>B):PStateful[T,B]	=
						it(_) map { case (t,a) => (t,func(a)) }
			}
			
	implicit def FStatefulCanMap[F[_]:CanMap,T,X]:CanMap[ ({type l[Y]=FStateful[F,T,Y]})#l ]	=
			new CanMap[ ({type l[Y]=FStateful[F,T,Y]})#l ] {
				def map[A,B](it:FStateful[F,T,A])(func:A=>B):FStateful[F,T,B]	=
						t => (CanMap[F] map it(t)) { case (t,a) => (t,func(a)) }
			}
}

trait CanMapInstancesLow extends CanMapInstancesBottom {
	implicit def ISeqCanMap:CanMap[ISeq]	=
			new CanMap[ISeq] {
				def map[A,B](it:ISeq[A])(func:A=>B):ISeq[B]		= it map func
			}
}

trait CanMapInstancesBottom {
	implicit def IdentityCanMap:CanMap[Identity]	=
			new CanMap[Identity] {
				def map[A,B](it:Identity[A])(func:A=>B):Identity[B]	= func(it)
			}
}
