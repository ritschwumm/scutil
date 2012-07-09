package scutil.data

object Tuples {
	def append2[A,Z]				(in:Product1[A],				last:Z):Tuple2[A,Z]					= Tuple2(in._1,														last)
	def append3[A,B,Z]				(in:Product2[A,B],				last:Z):Tuple3[A,B,Z]				= Tuple3(in._1, in._2,												last)
	def append4[A,B,C,Z]			(in:Product3[A,B,C],			last:Z):Tuple4[A,B,C,Z]				= Tuple4(in._1, in._2, in._3,										last)
	def append5[A,B,C,D,Z]			(in:Product4[A,B,C,D],			last:Z):Tuple5[A,B,C,D,Z]			= Tuple5(in._1, in._2, in._3, in._4,								last)
	def append6[A,B,C,D,E,Z]		(in:Product5[A,B,C,D,E],		last:Z):Tuple6[A,B,C,D,E,Z]			= Tuple6(in._1, in._2, in._3, in._4, in._5,							last)
	def append7[A,B,C,D,E,F,Z]		(in:Product6[A,B,C,D,E,F],		last:Z):Tuple7[A,B,C,D,E,F,Z]		= Tuple7(in._1, in._2, in._3, in._4, in._5, in._6,					last)
	def append8[A,B,C,D,E,F,G,Z]	(in:Product7[A,B,C,D,E,F,G],	last:Z):Tuple8[A,B,C,D,E,F,G,Z]		= Tuple8(in._1, in._2, in._3, in._4, in._5, in._6, in._7,			last)
	def append9[A,B,C,D,E,F,G,H,Z]	(in:Product8[A,B,C,D,E,F,G,H],	last:Z):Tuple9[A,B,C,D,E,F,G,H,Z]	= Tuple9(in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8,	last)
	
	def prepend2[A,Z]				(in:Product1[A],				first:Z):Tuple2[Z,A]				= Tuple2(first, in._1)
	def prepend3[A,B,Z]				(in:Product2[A,B],				first:Z):Tuple3[Z,A,B]				= Tuple3(first, in._1, in._2)
	def prepend4[A,B,C,Z]			(in:Product3[A,B,C],			first:Z):Tuple4[Z,A,B,C]			= Tuple4(first, in._1, in._2, in._3)
	def prepend5[A,B,C,D,Z]			(in:Product4[A,B,C,D],			first:Z):Tuple5[Z,A,B,C,D]			= Tuple5(first, in._1, in._2, in._3, in._4)
	def prepend6[A,B,C,D,E,Z]		(in:Product5[A,B,C,D,E],		first:Z):Tuple6[Z,A,B,C,D,E]		= Tuple6(first, in._1, in._2, in._3, in._4, in._5)
	def prepend7[A,B,C,D,E,F,Z]		(in:Product6[A,B,C,D,E,F],		first:Z):Tuple7[Z,A,B,C,D,E,F]		= Tuple7(first, in._1, in._2, in._3, in._4, in._5, in._6)
	def prepend8[A,B,C,D,E,F,G,Z]	(in:Product7[A,B,C,D,E,F,G],	first:Z):Tuple8[Z,A,B,C,D,E,F,G]	= Tuple8(first, in._1, in._2, in._3, in._4, in._5, in._6, in._7)
	def prepend9[A,B,C,D,E,F,G,H,Z]	(in:Product8[A,B,C,D,E,F,G,H],	first:Z):Tuple9[Z,A,B,C,D,E,F,G,H]	= Tuple9(first, in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8)

	//------------------------------------------------------------------------------

	type LCurried2[A,B]					= (A,B)
	type LCurried3[A,B,C]				= (A,(B,C))
	type LCurried4[A,B,C,D]				= (A,(B,(C,D)))
	type LCurried5[A,B,C,D,E]			= (A,(B,(C,(D,E))))
	type LCurried6[A,B,C,D,E,F]			= (A,(B,(C,(D,(E,F)))))
	type LCurried7[A,B,C,D,E,F,G]		= (A,(B,(C,(D,(E,(F,G))))))
	type LCurried8[A,B,C,D,E,F,G,H]		= (A,(B,(C,(D,(E,(F,(G,H)))))))
	type LCurried9[A,B,C,D,E,F,G,H,I]	= (A,(B,(C,(D,(E,(F,(G,(H,I))))))))
	
	def lcurry2[A,B]				(in:Product2[A,B]):					LCurried2[A,B]					= (in._1, in._2)
	def lcurry3[A,B,C]				(in:Product3[A,B,C]):				LCurried3[A,B,C]				= (in._1, (in._2, in._3))
	def lcurry4[A,B,C,D]			(in:Product4[A,B,C,D]):				LCurried4[A,B,C,D]				= (in._1, (in._2, (in._3, in._4)))
	def lcurry5[A,B,C,D,E]			(in:Product5[A,B,C,D,E]):			LCurried5[A,B,C,D,E]			= (in._1, (in._2, (in._3, (in._4, in._5))))
	def lcurry6[A,B,C,D,E,F]		(in:Product6[A,B,C,D,E,F]):			LCurried6[A,B,C,D,E,F]			= (in._1, (in._2, (in._3, (in._4, (in._5, in._6)))))
	def lcurry7[A,B,C,D,E,F,G]		(in:Product7[A,B,C,D,E,F,G]):		LCurried7[A,B,C,D,E,F,G]		= (in._1, (in._2, (in._3, (in._4, (in._5, (in._6, in._7))))))
	def lcurry8[A,B,C,D,E,F,G,H]	(in:Product8[A,B,C,D,E,F,G,H]):		LCurried8[A,B,C,D,E,F,G,H]		= (in._1, (in._2, (in._3, (in._4, (in._5, (in._6, (in._7, in._8)))))))
	def lcurry9[A,B,C,D,E,F,G,H,I]	(in:Product9[A,B,C,D,E,F,G,H,I]):	LCurried9[A,B,C,D,E,F,G,H,I]	= (in._1, (in._2, (in._3, (in._4, (in._5, (in._6, (in._7, (in._8, in._9))))))))

	def luncurry2[A,B]				(in:LCurried2[A,B]):				Tuple2[A,B]					= Tuple2(in._1,	in._2)
	def luncurry3[A,B,C]			(in:LCurried3[A,B,C]):				Tuple3[A,B,C]				= Tuple3(in._1, in._2._1, in._2._2)
	def luncurry4[A,B,C,D]			(in:LCurried4[A,B,C,D]):			Tuple4[A,B,C,D]				= Tuple4(in._1, in._2._1, in._2._2._1, in._2._2._2)
	def luncurry5[A,B,C,D,E]		(in:LCurried5[A,B,C,D,E]):			Tuple5[A,B,C,D,E]			= Tuple5(in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2)
	def luncurry6[A,B,C,D,E,F]		(in:LCurried6[A,B,C,D,E,F]):		Tuple6[A,B,C,D,E,F]			= Tuple6(in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2)
	def luncurry7[A,B,C,D,E,F,G]	(in:LCurried7[A,B,C,D,E,F,G]):		Tuple7[A,B,C,D,E,F,G]		= Tuple7(in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2)
	def luncurry8[A,B,C,D,E,F,G,H]	(in:LCurried8[A,B,C,D,E,F,G,H]):	Tuple8[A,B,C,D,E,F,G,H]		= Tuple8(in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2)
	def luncurry9[A,B,C,D,E,F,G,H,I](in:LCurried9[A,B,C,D,E,F,G,H,I]):	Tuple9[A,B,C,D,E,F,G,H,I]	= Tuple9(in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2)
	
	//------------------------------------------------------------------------------
	
	type RCurried2[A,B]					= (A,B)
	type RCurried3[A,B,C]				= ((A,B),C)
	type RCurried4[A,B,C,D]				= (((A,B),C),D)
	type RCurried5[A,B,C,D,E]			= ((((A,B),C),D),E)
	type RCurried6[A,B,C,D,E,F]			= (((((A,B),C),D),E),F)
	type RCurried7[A,B,C,D,E,F,G]		= ((((((A,B),C),D),E),F),G)
	type RCurried8[A,B,C,D,E,F,G,H]		= (((((((A,B),C),D),E),F),G),H)
	type RCurried9[A,B,C,D,E,F,G,H,I]	= ((((((((A,B),C),D),E),F),G),H),I)
	
	def rcurry2[A,B]				(in:Product2[A,B]):					RCurried2[A,B]					= (			in._1, in._2)
	def rcurry3[A,B,C]				(in:Product3[A,B,C]):				RCurried3[A,B,C]				= ((		in._1, in._2), in._3)
	def rcurry4[A,B,C,D]			(in:Product4[A,B,C,D]):				RCurried4[A,B,C,D]				= (((		in._1, in._2), in._3), in._4)
	def rcurry5[A,B,C,D,E]			(in:Product5[A,B,C,D,E]):			RCurried5[A,B,C,D,E]			= ((((		in._1, in._2), in._3), in._4), in._5)
	def rcurry6[A,B,C,D,E,F]		(in:Product6[A,B,C,D,E,F]):			RCurried6[A,B,C,D,E,F]			= (((((		in._1, in._2), in._3), in._4), in._5), in._6)
	def rcurry7[A,B,C,D,E,F,G]		(in:Product7[A,B,C,D,E,F,G]):		RCurried7[A,B,C,D,E,F,G]		= ((((((	in._1, in._2), in._3), in._4), in._5), in._6), in._7)
	def rcurry8[A,B,C,D,E,F,G,H]	(in:Product8[A,B,C,D,E,F,G,H]):		RCurried8[A,B,C,D,E,F,G,H]		= (((((((	in._1, in._2), in._3), in._4), in._5), in._6), in._7), in._8)
	def rcurry9[A,B,C,D,E,F,G,H,I]	(in:Product9[A,B,C,D,E,F,G,H,I]):	RCurried9[A,B,C,D,E,F,G,H,I]	= ((((((((	in._1, in._2), in._3), in._4), in._5), in._6), in._7), in._8), in._9)

	def runcurry2[A,B]				(in:RCurried2[A,B]):				Tuple2[A,B]					= Tuple2(in._1,																																											in._2)
	def runcurry3[A,B,C]			(in:RCurried3[A,B,C]):				Tuple3[A,B,C]				= Tuple3(in._1._1,																																							in._1._2,	in._2)
	def runcurry4[A,B,C,D]			(in:RCurried4[A,B,C,D]):			Tuple4[A,B,C,D]				= Tuple4(in._1._1._1, 																																		in._1._1._2,	in._1._2,	in._2)
	def runcurry5[A,B,C,D,E]		(in:RCurried5[A,B,C,D,E]):			Tuple5[A,B,C,D,E]			= Tuple5(in._1._1._1._1,																													in._1._1._1._2,	in._1._1._2,	in._1._2,	in._2)
	def runcurry6[A,B,C,D,E,F]		(in:RCurried6[A,B,C,D,E,F]):		Tuple6[A,B,C,D,E,F]			= Tuple6(in._1._1._1._1._1,																								in._1._1._1._1._2,	in._1._1._1._2,	in._1._1._2,	in._1._2,	in._2)
	def runcurry7[A,B,C,D,E,F,G]	(in:RCurried7[A,B,C,D,E,F,G]):		Tuple7[A,B,C,D,E,F,G]		= Tuple7(in._1._1._1._1._1._1,																	in._1._1._1._1._1._2,	in._1._1._1._1._2,	in._1._1._1._2,	in._1._1._2,	in._1._2,	in._2)
	def runcurry8[A,B,C,D,E,F,G,H]	(in:RCurried8[A,B,C,D,E,F,G,H]):	Tuple8[A,B,C,D,E,F,G,H]		= Tuple8(in._1._1._1._1._1._1._1,									in._1._1._1._1._1._1._2,	in._1._1._1._1._1._2,	in._1._1._1._1._2,	in._1._1._1._2,	in._1._1._2,	in._1._2,	in._2)
	def runcurry9[A,B,C,D,E,F,G,H,I](in:RCurried9[A,B,C,D,E,F,G,H,I]):	Tuple9[A,B,C,D,E,F,G,H,I]	= Tuple9(in._1._1._1._1._1._1._1._1,	in._1._1._1._1._1._1._1._2,	in._1._1._1._1._1._1._2,	in._1._1._1._1._1._2,	in._1._1._1._1._2,	in._1._1._1._2,	in._1._1._2,	in._1._2,	in._2)
	
	//------------------------------------------------------------------------------
	
	// lflip	== luncurry andThen rcurry
	def lflip2[A,B]					(in:LCurried2[A,B]):				RCurried2[A,B]					= in
	def lflip3[A,B,C]				(in:LCurried3[A,B,C]):				RCurried3[A,B,C]				= ((		in._1, in._2._1), in._2._2)
	def lflip4[A,B,C,D]				(in:LCurried4[A,B,C,D]):			RCurried4[A,B,C,D]				= (((		in._1, in._2._1), in._2._2._1), in._2._2._2)
	def lflip5[A,B,C,D,E]			(in:LCurried5[A,B,C,D,E]):			RCurried5[A,B,C,D,E]			= ((((		in._1, in._2._1), in._2._2._1), in._2._2._2._1), in._2._2._2._2)
	def lflip6[A,B,C,D,E,F]			(in:LCurried6[A,B,C,D,E,F]):		RCurried6[A,B,C,D,E,F]			= (((((		in._1, in._2._1), in._2._2._1), in._2._2._2._1), in._2._2._2._2._1), in._2._2._2._2._2)
	def lflip7[A,B,C,D,E,F,G]		(in:LCurried7[A,B,C,D,E,F,G]):		RCurried7[A,B,C,D,E,F,G]		= ((((((	in._1, in._2._1), in._2._2._1), in._2._2._2._1), in._2._2._2._2._1), in._2._2._2._2._2._1), in._2._2._2._2._2._2)
	def lflip8[A,B,C,D,E,F,G,H]		(in:LCurried8[A,B,C,D,E,F,G,H]):	RCurried8[A,B,C,D,E,F,G,H]		= (((((((	in._1, in._2._1), in._2._2._1), in._2._2._2._1), in._2._2._2._2._1), in._2._2._2._2._2._1), in._2._2._2._2._2._2._1), in._2._2._2._2._2._2._2)
	def lflip9[A,B,C,D,E,F,G,H,I]	(in:LCurried9[A,B,C,D,E,F,G,H,I]):	RCurried9[A,B,C,D,E,F,G,H,I]	= ((((((((	in._1, in._2._1), in._2._2._1), in._2._2._2._1), in._2._2._2._2._1), in._2._2._2._2._2._1), in._2._2._2._2._2._2._1), in._2._2._2._2._2._2._2._1), in._2._2._2._2._2._2._2._2)
	
	// rflip	== runcurry andThen lcurry
	def rflip[A,B]					(in:RCurried2[A,B]):				LCurried2[A,B]					= in
	def rflip3[A,B,C]				(in:RCurried3[A,B,C]):				LCurried3[A,B,C]				= 																																							(in._1._1,		(in._1._2, in._2))
	def rflip4[A,B,C,D]				(in:RCurried4[A,B,C,D]):			LCurried4[A,B,C,D]				= 																																		(in._1._1._1,		(in._1._1._2,	(in._1._2, in._2)))
	def rflip5[A,B,C,D,E]			(in:RCurried5[A,B,C,D,E]):			LCurried5[A,B,C,D,E]			= 																													(in._1._1._1._1,	(in._1._1._1._2,	(in._1._1._2,	(in._1._2, in._2))))
	def rflip6[A,B,C,D,E,F]			(in:RCurried6[A,B,C,D,E,F]):		LCurried6[A,B,C,D,E,F]			= 																							(in._1._1._1._1._1,		(in._1._1._1._1._2,	(in._1._1._1._2,	(in._1._1._2,	(in._1._2, in._2)))))
	def rflip7[A,B,C,D,E,F,G]		(in:RCurried7[A,B,C,D,E,F,G]):		LCurried7[A,B,C,D,E,F,G]		= 																(in._1._1._1._1._1._1,		(in._1._1._1._1._1._2,	(in._1._1._1._1._2,	(in._1._1._1._2,	(in._1._1._2,	(in._1._2, in._2))))))
	def rflip8[A,B,C,D,E,F,G,H]		(in:RCurried8[A,B,C,D,E,F,G,H]):	LCurried8[A,B,C,D,E,F,G,H]		= 								(in._1._1._1._1._1._1._1,		(in._1._1._1._1._1._1._2,	(in._1._1._1._1._1._2,	(in._1._1._1._1._2,	(in._1._1._1._2,	(in._1._1._2,	(in._1._2, in._2)))))))
	def rflip9[A,B,C,D,E,F,G,H,I]	(in:RCurried9[A,B,C,D,E,F,G,H,I]):	LCurried9[A,B,C,D,E,F,G,H,I]	= (in._1._1._1._1._1._1._1._1,	(in._1._1._1._1._1._1._1._2,	(in._1._1._1._1._1._1._2,	(in._1._1._1._1._1._2,	(in._1._1._1._1._2,	(in._1._1._1._2,	(in._1._1._2,	(in._1._2, in._2))))))))
}
