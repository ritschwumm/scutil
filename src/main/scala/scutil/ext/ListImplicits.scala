package scutil.ext

object ListImplicits extends ListImplicits

trait ListImplicits {
	implicit def toListExt[T](delegate:List[T])	= new ListExt(delegate)
}

final class ListExt[T](delegate:List[T]) {
	/**
	calculate common prefix and differing tails for two lists
	usage example: 
		List(1,2,3,4) unprefix List(1,2,4,5)
		==> Triple(List(1,2), List(3,4), List(4,5))
	*/
	def unprefix(other:List[T]):Triple[List[T],List[T],List[T]]	= 
			ListUtil unprefix (delegate, other)
}

private object ListUtil {
	/**
	calculate common prefix and differing tails for two lists
	usage example: 
		unprefix(List(1,2,3,4), List(1,2,4,5))
		==> Triple(List(1,2), List(3,4), List(4,5))
	*/
	def unprefix[T](list1:List[T], list2:List[T]):Triple[List[T],List[T],List[T]]	= {
		var	cur1	= list1
		var cur2	= list2
		var prefix	= Nil:List[T]
		while (cur1.nonEmpty && cur2.nonEmpty && cur1.head == cur2.head) {
			prefix	= cur1.head :: prefix
			cur1	= cur1.tail
			cur2	= cur2.tail
		}
		Triple(prefix.reverse, cur1, cur2)
	}
}
