package scutil

import java.lang.{ 
	Number	=> JNumber 
}
import java.math.{
	BigDecimal	=> JBigDecimal,
	BigInteger	=> JBigInteger,
	MathContext,
	RoundingMode
}

import scutil.ext.StringImplicits._

object BigRational {
	/** additive neutral element */
	val ZERO	= new BigRational(JBigInteger.ZERO, JBigInteger.ONE)
	/** multiplicative neutral element */
	val ONE		= new BigRational(JBigInteger.ONE,  JBigInteger.ONE)	
	
	def apply(numerator:JBigInteger, denominator:JBigInteger):BigRational = 
			new BigRational(numerator, denominator)
			
	def apply(numerator:JBigDecimal):BigRational = 
			new BigRational(
					numerator.unscaledValue,
					(JBigDecimal.ONE scaleByPowerOfTen numerator.scale).toBigInteger)
					
	def apply(numerator:Long, denominator:Long):BigRational = 
			new BigRational(
					JBigInteger valueOf numerator,
					JBigInteger valueOf denominator)
					
	def apply(numerator:Long):BigRational = 
			new BigRational(
					JBigInteger valueOf numerator,
					JBigInteger.ONE)
					
	def unapply(self:BigRational):Option[(JBigInteger,JBigInteger)] = 
			Some((self.numerator, self.denominator)) 
					
	/** parse the output of #toString */
	def parse(s:String):Option[BigRational] = 
			s splitAround '/' match {
				case Seq(num,den)	=> 
					try { Some(new BigRational(new JBigInteger(num), new JBigInteger(den))) }
					catch { case e:NumberFormatException	=> None }
				case _	=> None
			}
}

/** an immutable, auto-simplifying BigInteger fraction */
final class BigRational(_numerator:JBigInteger, _denominator:JBigInteger) extends JNumber with Ordered[BigRational] {
	if (_denominator == JBigInteger.ZERO)	throw new ArithmeticException("denominator must not be zero")
	
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
	def signum:Int	= (numerator compareTo	JBigInteger.ZERO)
	
	def integralValue:JBigInteger		= numerator divide		denominator
	def integralRemainder:JBigInteger	= numerator remainder	denominator
	
	def integralValueAndRemainder:(JBigInteger,JBigInteger)	= {
		val	both	= numerator divideAndRemainder denominator
		(both(0), both(1))
	}
	 
	//------------------------------------------------------------------------------
	//## comparison
	
	def min(that:BigRational):BigRational = if (this < that) this else that
	def max(that:BigRational):BigRational = if (this > that) this else that
	
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
	
	/*
	def toBigDecimal(scale:Int, roundingMode:RoundingMode):JBigDecimal =
			new JBigDecimal(numerator) divide (new JBigDecimal(denominator), scale, roundingMode)
	*/
	
	def toBigDecimal(mathContext:MathContext):JBigDecimal =
			new JBigDecimal(numerator) divide (new JBigDecimal(denominator), mathContext)
			
	// TODO weak 
	override def doubleValue:Double	= numerator.doubleValue / denominator.doubleValue
	override def floatValue:Float	= numerator.floatValue  / denominator.floatValue
	override def intValue:Int		= scala.math.round(doubleValue).toInt
	override def longValue:Long		= scala.math.round(doubleValue)
	
	override def toString:String	= numerator + "/" + denominator
}
