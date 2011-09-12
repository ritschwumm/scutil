package scutil.ext

import scutil.Tuples

object SeqImplicits extends SeqImplicits

trait SeqImplicits {
	implicit def toSeqExt[S](delegate:Seq[S])	= new SeqExt(delegate)
}

final class SeqExt[S](delegate:Seq[S]) {
	/** distinct with a custom equality check */ 
	def distinctWith(same:(S,S)=>Boolean):Seq[S] = 
			( delegate.toList.foldLeft(List.empty[S]) { (retained:List[S],candidate:S) =>
				retained find { same(_,candidate) } match {
					case Some(_)	=> retained
					case None		=> candidate :: retained
				}
			}).toList.reverse
	
	/** distinct on a single property */ 
	def distinctBy[T](extract:S=>T):Seq[S] = 
			distinctWith { extract(_) == extract(_) }
	
	/** insert a separator between elements */	
	def intersperse[S1>:S](separator:S1):Seq[S1]	=
			if (delegate.nonEmpty)	delegate flatMap { Seq(separator, _) } tail
			else					delegate
			
	/** triple every item with its previous and next item */
	def adjacents:Seq[(Option[S],S,Option[S])]	= {
		val somes	= delegate map Some.apply
		val prevs	= None +: (somes dropRight 1) 
		val nexts	= (somes drop 1) :+ None
		prevs zip delegate zip nexts map Tuples.runcurry3
	}
}
