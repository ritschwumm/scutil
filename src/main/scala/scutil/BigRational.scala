package scutil

import java.math.{ BigDecimal => BigDec }
import java.math.{ BigInteger => BigInt }
import java.math.MathContext
import java.math.RoundingMode

import scutil.ext.StringImplicits._

object BigRational {
	/** additive neutral element */
	val ZERO	= new BigRational(BigInt.ZERO, BigInt.ONE)
	/** multiplicative neutral element */
	val ONE		= new BigRational(BigInt.ONE,  BigInt.ONE)	
	
	def apply(numerator:BigInt, denominator:BigInt):BigRational = 
			new BigRational(numerator, denominator)
			
	def apply(numerator:BigDec):BigRational = 
			new BigRational(
					numerator.unscaledValue,
					BigDec.ONE scaleByPowerOfTen numerator.scale toBigInteger)
					
	def apply(numerator:Long, denominator:Long):BigRational = 
			new BigRational(
					BigInt.valueOf(numerator),
					BigInt.valueOf(denominator))
					
	def apply(numerator:Long):BigRational = 
			new BigRational(
					BigInt.valueOf(numerator),
					BigInt.ONE)
					
	def unapply(self:BigRational):Option[(BigInt,BigInt)] = 
			Some((self.numerator, self.denominator)) 
					
	/** parse the output of #toString */
	def parse(s:String):Option[BigRational] = s splitAround '/' match {
		case Seq(num,den)	=> Some(new BigRational(new BigInt(num), new BigInt(den)))
		case _				=> None
	}
}

/** an immutable, auto-simplifying BigInteger fraction */
final class BigRational(_numerator:BigInt, _denominator:BigInt) extends java.lang.Number with Ordered[BigRational] {
	if (_denominator == BigInt.ZERO)	throw new ArithmeticException("denominator must not be zero")
	
	// simplify
	val (numerator,denominator) = {
		 val gcd	= _numerator gcd _denominator
		 (
			(if (_denominator.signum == -1)	_numerator.negate else _numerator) divide gcd, 
			_denominator.abs divide gcd
		)
	}
	
	//------------------------------------------------------------------------------
	//## size
	
	def proper:Boolean	= (numerator.abs compareTo denominator.abs) == -1
	
	//------------------------------------------------------------------------------
	//## calculation
	
	def +(that:BigRational):BigRational =
				 if (this == BigRational.ZERO)	that
			else if (that == BigRational.ZERO)	this
			else new BigRational(
					(this.numerator multiply that.denominator) add (this.denominator multiply that.numerator),
					this.denominator multiply that.denominator)

	def -(that:BigRational):BigRational =
				 if (this == BigRational.ZERO)	that.negate
			else if (that == BigRational.ZERO)	this
			else new BigRational(
					(this.numerator multiply that.denominator) subtract (this.denominator multiply that.numerator),
					this.denominator multiply that.denominator)
		
	def *(that:BigRational):BigRational =
				 if (this == BigRational.ONE)	that
			else if (that == BigRational.ONE)	this
			else new BigRational(
					this.numerator		multiply that.numerator,
					this.denominator	multiply that.denominator)
	
	def /(that:BigRational):BigRational =
				 if (this == BigRational.ONE)	that.reciprocal
			else if (that == BigRational.ONE)	this
			else new BigRational(
					this.numerator		multiply that.denominator,
					this.denominator	multiply that.numerator)
	
	/** additive inverse */
	def negate:BigRational = 
			if (this == BigRational.ZERO)	this
			else new BigRational(
					numerator.negate, 
					denominator)

	/** multiplicative inverse */
	def reciprocal:BigRational =
			if (this == BigRational.ONE)	this
			else new BigRational(
					denominator, 
					numerator)
	
	/** absolute value */
	def abs:BigRational = 
			if (signum >= 0)	this
			else				negate

	/** -1 for negative, 0 for zero, +1 for positive */
	def signum:Int					= (numerator compareTo	BigInt.ZERO)
	
	def integralValue:BigInt		= numerator divide		denominator
	def integralRemainder:BigInt	= numerator remainder	denominator
	
	def integralValueAndRemainder:(BigInt,BigInt)	= {
		val	both	= numerator divideAndRemainder denominator
		(both(0), both(1))
	}
	 
	//------------------------------------------------------------------------------
	//## comparison
	
	def min(that:BigRational):BigRational = if (this < that) this else that
	def max(that:BigRational):BigRational = if (this > that) this else that
	
	/*
	// imlemented by the Ordered trait
	def <(that:BigRational):Boolean		= (this compareTo that) == -1
	def >(that:BigRational):Boolean		= (this compareTo that) == +1
	def <=(that:BigRational):Boolean	= (this compareTo that) != +1
	def >=(that:BigRational):Boolean	= (this compareTo that) != -1
	*/
	
	def compare(that:BigRational):Int =
			if (this == that)	0
			else 				(this.numerator multiply that.denominator) compareTo (that.numerator multiply this.denominator)
	
	override def equals(that:Any)	= that match {
	 	case x:BigRational	=> this equalsTo x 
	 	case _				=> false
	}
			
	def equalsTo(that:BigRational):Boolean = 
			this.numerator		== that.numerator	&& 
			this.denominator	== that.denominator
			
	override def hashCode: Int 	= 
			numerator.hashCode ^ denominator.hashCode
	
	//------------------------------------------------------------------------------
	//## conversion
	
	def toBigDecimal(scale:Int, roundingMode:RoundingMode):BigDec =
			new BigDec(numerator) divide (new BigDec(denominator), scale, roundingMode)
	
	def toBigDecimal(mathContext:MathContext):BigDec =
			new BigDec(numerator) divide (new BigDec(denominator), mathContext)
			
	// TODO weak 
	override def doubleValue:Double	= numerator.doubleValue / denominator.doubleValue
	override def floatValue:Float	= numerator.floatValue  / denominator.floatValue
	override def intValue:Int		= scala.math.round(doubleValue) toInt
	override def longValue:Long		= scala.math.round(doubleValue)
	
	override def toString:String	= numerator + "/" + denominator
}
