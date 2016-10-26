package scutil.math

import org.specs2.mutable._

import scutil.{ math => self }

import scutil.math.implicits._

class MathTest extends Specification {
	"Ordering" should {
		"be contravariant" in {
			trait Numb
			trait Inte extends Numb
			
			val i:Inte	= null
			val n:Numb	= i
			
			val no:Ordering[Numb]	= null
			val io:Ordering[Inte]	= no.vary[Inte]
			
			success
		}
	}
	
	"modulo" should {
		"work for positive modulos" in {
			((-6 until 6).toVector map { (i:Int) => self moduloInt (i, 3) }) mustEqual Vector(0,1,2,0,1,2,0,1,2,0,1,2)
		}
		"work for negative modulos" in {
			((-6 until 6).toVector map { (i:Int) => self moduloInt (i, -3) }) mustEqual Vector(0,-2,-1,0,-2,-1,0,-2,-1,0,-2,-1)
		}
	}
}
