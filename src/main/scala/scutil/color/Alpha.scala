package scutil.color

import scala.math._

import scutil.math

/** value range is 0..1 */
final case class Alpha(a:Float) {
	def blend(that:Alpha, ratio:Float):Alpha	= 
			Alpha(math blend (this.a, that.a, ratio))
	
	def diff(that:Alpha):Float	= 
			abs(this.a - that.a)
		
	def inverse:Alpha	= Alpha(1-a)
}
