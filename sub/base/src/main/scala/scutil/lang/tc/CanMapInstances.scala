package scutil.lang.tc

import scutil.lang._

trait CanMapInstances extends CanMapInstancesLow {
	//------------------------------------------------------------------------------
	//## builtin
	
	implicit def FunctionCanMap[S]:CanMap[ ({type l[T]=Function[S,T]})#l ]	=
			new CanMap[ ({type l[T]=Function[S,T]})#l ] {
				def map[A,B](func:A=>B):Function[S,A]=>Function[S,B]	= _ andThen func
			}
			
	implicit def PairCanMap[S]:CanMap[ ({type l[T]=(S,T)})#l ]	=
			new CanMap[ ({type l[T]=(S,T)})#l ] {
				def map[A,B](func:A=>B):((S,A))=>(S,B)				= (it:(S,A)) => (it._1, func(it._2))
			}
			
	implicit def EitherCanMap[S]:CanMap[ ({type l[T]=Either[S,T]})#l ]	=
			new CanMap[ ({type l[T]=Either[S,T]})#l ] {
				def map[A,B](func:A=>B):Either[S,A]=>Either[S,B]	= _.right map func
			}
			
	implicit def OptionCanMap:CanMap[Option]	=
			new CanMap[Option] {
				def map[A,B](func:A=>B):Option[A]=>Option[B]	= _ map func
			}
			
	implicit def VectorCanMap:CanMap[Vector]	=
			new CanMap[Vector] {
				def map[A,B](func:A=>B):Vector[A]=>Vector[B]	= _ map func
			}
			
	implicit def ListCanMap:CanMap[List]	=
			new CanMap[List] {
				def map[A,B](func:A=>B):List[A]=>List[B]		= _ map func
			}
			
	//------------------------------------------------------------------------------
	//## own
			
	implicit def ConverterCanMap[E,S]:CanMap[ ({type l[T]=Converter[E,S,T]})#l ]	=
			new CanMap[ ({type l[T]=Converter[E,S,T]})#l ] {
				def map[A,B](func:A=>B):Converter[E,S,A]=>Converter[E,S,B]	= _ map func
			}
			
	implicit def ExtractorCanMap[S]:CanMap[ ({type l[T]=Extractor[S,T]})#l ]	=
			new CanMap[ ({type l[T]=Extractor[S,T]})#l ] {
				def map[A,B](func:A=>B):Extractor[S,A]=>Extractor[S,B]		= _ map func
			}
			
	implicit def NesCanMap:CanMap[Nes]	=
			new CanMap[Nes] {
				def map[A,B](func:A=>B):Nes[A]=>Nes[B]						= _ map func
			}
			
	implicit def StoreCanMap[S]:CanMap[ ({type l[T]=Store[T,S]})#l ]	=
			new CanMap[ ({type l[T]=Store[T,S]})#l ] {
				def map[A,B](func:A=>B):Store[A,S]=>Store[B,S]				= _ map func
			}
			
	implicit def TriedCanMap[S]:CanMap[ ({type l[T]=Tried[S,T]})#l ]	=
			new CanMap[ ({type l[T]=Tried[S,T]})#l ] {
				def map[A,B](func:A=>B):Tried[S,A]=>Tried[S,B]				= _ map func
			}
			
	implicit def ValidatedCanMap[S]:CanMap[ ({type l[T]=Validated[S,T]})#l ]	=
			new CanMap[ ({type l[T]=Validated[S,T]})#l ] {
				def map[A,B](func:A=>B):Validated[S,A]=>Validated[S,B]		= _ map func
			}
}

trait CanMapInstancesLow extends CanMapInstancesBottom {
	implicit def ISeqCanMap:CanMap[ISeq]	=
			new CanMap[ISeq] {
				def map[A,B](func:A=>B):ISeq[A]=>ISeq[B]		= _ map func
			}
}

trait CanMapInstancesBottom {
	implicit def IdentityCanMap:CanMap[Identity]	=
			new CanMap[Identity] {
				def map[A,B](func:A=>B):Identity[A]=>Identity[B]	= func(_)
			}
}
