/*
package scutil.lang

import scutil.lang.tc._

trait ApplicativeGenerated {
	//------------------------------------------------------------------------------
	// zip
	
	def zip2
	[2..#def zip1[L,[#R1#]]([#r1:Either[L,R1]#]):Either[L,([#R1#])]	=
		[#r1# zip ] map assoc.unarrow1#
	]
	
	//------------------------------------------------------------------------------
	// lift
	
	[2..#def lift1[L,[#R1#],RR](func:([#R1#])=>RR):([#Either[L,R1]#])=>Either[L,RR]	=
			([#r1:Either[L,R1]#]) => Either right func.curried ap [#r1# ap ]#
	]
	
	//------------------------------------------------------------------------------
	// map
	
	[2..#def map1[L,[#R1#],RR]([#r1:Either[L,R1]#]):(([#R1#])=>RR)=>Either[L,RR]	=
			func => Either right func.curried ap [#r1# ap ]#
	]
}
*/