package scutil.lang

object Tuples extends TuplesGenerated {
	/*
	// these we get from TuplesGenerated
	
	type LCurried4[A,B,C,D]	= (A,(B,(C,D)))
	type RCurried4[A,B,C,D]	= (((A,B),C),D)
	
	def lcurry4[A,B,C,D]	(in:Product4[A,B,C,D]):	LCurried4[A,B,C,D]	= (in._1, (in._2, (in._3, in._4)))
	def rcurry4[A,B,C,D]	(in:Product4[A,B,C,D]):	RCurried4[A,B,C,D]	= ((( in._1, in._2), in._3), in._4)
	
	def luncurry4[A,B,C,D]	(in:LCurried4[A,B,C,D]):Tuple4[A,B,C,D]		= Tuple4(in._1, in._2._1, in._2._2._1, in._2._2._2)
	def runcurry4[A,B,C,D]	(in:RCurried4[A,B,C,D]):Tuple4[A,B,C,D]		= Tuple4(in._1._1._1, 																																		in._1._1._2,	in._1._2,	in._2)
	
	def lflip4[A,B,C,D]		(in:LCurried4[A,B,C,D]):RCurried4[A,B,C,D]	= (((in._1, in._2._1), in._2._2._1), in._2._2._2)
	def rflip4[A,B,C,D]		(in:RCurried4[A,B,C,D]):LCurried4[A,B,C,D]	= (in._1._1._1,	(in._1._1._2, (in._1._2, in._2)))
	
	def reverse4[A,B,C,D](in:Product4[A,B,C,D]):Tuple4[D,C,B,A]			= (in._4,in._3,in._2,in._1)
	*/
}
