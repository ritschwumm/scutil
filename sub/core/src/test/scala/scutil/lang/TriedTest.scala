/*
package scutil

import org.specs2.mutable._

import Implicits._
import data._

class TriedTest extends Specification {
	"tried" should {
		"collect sucess if there is an implicit default available" in {
			val input:Tried[Char,Int]		= Win(1)
			implicit 
			val default:Tried[Char,Boolean]	= Fail('a')
			input collect { case 1 => true } mustEqual Win(true)
		}
		
		"collect failure if there is an implicit default available" in {
			val input:Tried[Char,Int]		= Win(2)
			implicit 
			val default:Tried[Char,Boolean]	= Fail('a')
			input collect { case 1 => true } mustEqual default
		}
		
		"filter sucess if there is an implicit default available" in {
			val input:Tried[Char,Int]	= Win(1)
			implicit 
			val default:Tried[Char,Int]	= Fail('a')
			input filter { _ == 1 } mustEqual input
		}
		
		"filter failure if there is an implicit default available" in {
			val input:Tried[Char,Int]	= Win(1)
			implicit 
			val default:Tried[Char,Int]	= Fail('a')
			input filter { _ == 2 } mustEqual default
		}
		
		"work in for-syntax using withFilter success" in {
			val input:Tried[Char,Int]	= Win(1)
			implicit 
			val default:Tried[Char,Int]	= Fail('a')
			val a	=
					for {
						x:Int	<- input
						if x == 1
					}	
					yield x
			a mustEqual input
		}
		
		"work in for-syntax using withFilter fail" in {
			val input:Tried[Char,Int]	= Win(1)
			implicit 
			val default:Tried[Char,Int]	= Fail('a')
			val a	=
					for {
						x:Int	<- input
						if x == 2
					}	
					yield x
			a mustEqual default
		}
	}
}
*/
