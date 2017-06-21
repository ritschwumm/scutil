package scutil.time

final case class Gregorian(day:Int, month:Int, year:Int) extends Ordered[Gregorian] {
	def compare(that:Gregorian):Int		= {
		val	y	= this.year		compare that.year;	if (y != 0)	return y
		val	m	= this.month	compare that.month;	if (m != 0)	return m
		val	d	= this.day		compare that.day;	if (d != 0)	return d
		0
	}
	
	def min(that:Gregorian):Gregorian	= if (this < that) this else that
	def max(that:Gregorian):Gregorian	= if (this > that) this else that
}
