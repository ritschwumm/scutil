package scutil.geom

object IntSpan {
	val zero	= IntSpan(0, 0)
}

case class IntSpan(start:Int, size:Int) {
	val end	= start + size
	
	def withStart(start:Int):IntSpan	= IntSpan(start,		size)
	def withEnd(end:Int):IntSpan		= IntSpan(end - size,	size)
	
	def unary_- :IntSpan		= negate
	def + (offset:Int):IntSpan	= move(offset)
	def - (offset:Int):IntSpan	= unmove(offset)
	
	def negate:IntSpan				= IntSpan(end,				-size)
	def move(offset:Int):IntSpan	= IntSpan(start + offset,	size)
	def unmove(offset:Int):IntSpan	= IntSpan(start - offset,	size)
	
	def normalize:IntSpan	=
			if (size >= 0)	this
			else			negate
	
	def restrictTo(that:IntSpan):IntSpan	=
				 if (this.size	> that.size)	that
			else if (this.start	< that.start)	this withStart	that.start
			else if (this.end	> that.end)		this withEnd	that.end
			else								this
			
	def rectWith(that:IntSpan):IntRect	=
			IntRect(this, that)
}
