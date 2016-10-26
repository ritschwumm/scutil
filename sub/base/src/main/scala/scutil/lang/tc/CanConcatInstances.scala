package scutil.lang.tc

import scutil.lang._
import scutil.lang.implicits._

trait CanConcatInstances extends CanConcatInstancesLow {
	//------------------------------------------------------------------------------
	//## builtin
	
	implicit val StringCanConcat:CanConcat[String]	=
			CanConcat by (_ + _)
		
	implicit def OptionCanConcat[T]:CanConcat[Option[T]]	=
			CanConcat by (_ orElse _)
		
	implicit def EitherCanConcat[S,T]:CanConcat[Either[S,T]]	=
			CanConcat by { (a,b) =>
				a match {
					case Left(_)	=> b
					case Right(_)	=> a
				}
			}
		
	implicit def VectorCanConcat[T]:CanConcat[Vector[T]]	=
			CanConcat by (_ ++ _)
	
	implicit def ListCanConcat[T]:CanConcat[List[T]]	=
			CanConcat by (_ ++ _)
		
	implicit def SetCanConcat[T]:CanConcat[Set[T]]	=
			CanConcat by (_ ++ _)
		
	implicit def MapCanConcat[K,V]:CanConcat[Map[K,V]]	=
			CanConcat by (_ ++ _)	
	
	//------------------------------------------------------------------------------
	//## own
	
	implicit def ConverterCanConcat[E:CanConcat,S,T]:CanConcat[Converter[E,S,T]]	=
			CanConcat by (_ orElse _)
		
	implicit def ExtractorCanConcat[S,T]:CanConcat[Extractor[S,T]]	=
			CanConcat by (_ orElse _)
		
	implicit def EndoCanConcat[T]:CanConcat[Endo[T]]	=
			CanConcat by (_ andThen _)
	
	implicit def NesCanConcat[T]:CanConcat[Nes[T]]	=
			CanConcat by (_ ++ _)
		
	implicit def PEndoCanConcat[T]:CanConcat[PEndo[T]]	=
			CanConcat by (_ andThenFixed _)
		
	implicit def PFunctionCanConcat[S,T]:CanConcat[PFunction[S,T]]	=
			CanConcat by (_ orElse _)
		
	implicit def PLensCanConcat[S,T]:CanConcat[PLens[S,T]]	=
			CanConcat by (_ orElse _)
	
	implicit def PrismCanConcat[S,T]:CanConcat[Prism[S,T]]	=
			CanConcat by (_ orElse _)
	
	implicit def TriedCanConcat[S,T]:CanConcat[Tried[S,T]]	=
			CanConcat by (_ orElse _)
	
	implicit def ValidatedCanConcat[S:CanConcat,T]:CanConcat[Validated[S,T]]	=
			CanConcat by (_ orElse _)
}

trait CanConcatInstancesLow extends CanConcatInstancesBottom {
	implicit def ISeqCanConcat[T]:CanConcat[ISeq[T]]	=
			CanConcat by (_ ++ _)
}

trait CanConcatInstancesBottom {}

