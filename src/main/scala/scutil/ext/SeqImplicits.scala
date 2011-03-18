package scutil.ext

object SeqImplicits extends SeqImplicits

trait SeqImplicits {
	implicit def toSeqExt[S](delegate:Seq[S])	= new SeqExt(delegate)
}

final class SeqExt[S](delegate:Seq[S]) {
	/*
	// == collect f
	def filterMap[T](f:PartialFunction[S,T]):Seq[T] = 
			// delegate flatMap (f.optional andThen Option.option2Iterable[T])
			delegate flatMap { element =>
				if (f isDefinedAt element)	List(f apply element)
				else						Nil
			}
	*/
	
	/** distinct with a custom equality predicate */ 
	def retainFirst(same:(S,S)=>Boolean):Seq[S] = 
			( delegate.toList.foldLeft(List[S]()) { (retained:List[S],candidate:S) =>
				retained find { same(_,candidate) } match {
					case Some(_)	=> retained
					case None		=> candidate :: retained
				}
			}).toList.reverse
}
