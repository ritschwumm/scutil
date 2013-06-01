package scutil.lens

import scutil.lang._
import scutil.Implicits._

object PLenses {
	def seq[T](i:Int):PLens[Seq[T],T]	=
			PLens(s => 
				s	containsIndex
				i	guard 
				Store[Seq[T],T](
					s(i),
					v => s updated (i, v)
				)
			)
			
	def map[K,V](k:K):PLens[Map[K,V],V]	=
			PLens(s => 
				s	contains
				k	guard 
				Store[Map[K,V],V](
					s(k),
					v => s updated (k, v)
				)
			)
}
