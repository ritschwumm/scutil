import sbt._

object Boilerplate {
	def generate(srcDir:File):Seq[File]	= {
		val	outDir	= srcDir / "boilerplate"
		outDir.mkdirs()
		
		Vector(
			Tried		genFile outDir,
			Validated	genFile outDir,
			Tuples		genFile outDir
		)
	}
	
	//------------------------------------------------------------------------------
	//## tried
	
	object Tried {
		def genFile(outDir:File):File	= {
			val outFile	= outDir / "TriedGenerated.scala"
			IO write (outFile,	getTrait)
			outFile
		}
		
		def getTrait:String	= {
			s"""
			package scutil.lang
			
			trait TriedGenerated {
				//------------------------------------------------------------------------------
				// zip
				${3 to 22 map genZip	mkString "\n"}
				//------------------------------------------------------------------------------
				// lift
				${2 to 22 map genLift	mkString "\n"}
			}
			"""
		}
		
		def genZip(arity:Int):String	= {
			val idxs	= 1 to arity
			s"""
			def zip${arity}
				[E,${csep(idxs map plainType)}]
				(${(csep(idxs map wrapInput))})
				:
				Tried[E,(${csep(idxs map plainType)})]
				=
				${idxs map argValue mkString " zip "} map Tuples.runcurry${arity}
			"""
		}
		
		def genLift(arity:Int):String	= {
			val idxs	= 1 to arity
			s"""
			def lift${arity}
				[E,${csep(idxs map plainType)},T]
				(func:(${csep(idxs map plainType)})=>T)
				:
				(${csep(idxs map wrapType)})=>Tried[E,T]
				=
				(${csep(idxs map wrapInput)})
				=>
				Tried win func.curried ap ${idxs map argValue mkString " ap "}
			"""
		}
		
		def plainType(idx:Int):String	= "S" + idx.toString
		def wrapType(idx:Int):String	= s"Tried[E,${plainType(idx)}]"
		def argValue(idx:Int):String	= "s" + idx.toString
		def plainInput(idx:Int):String	= argValue(idx) + ":" + plainType(idx)
		def wrapInput(idx:Int):String	= argValue(idx) + ":" + wrapType(idx)
		def csep(s:Seq[String]):String	= s mkString ","
	}
	
	//------------------------------------------------------------------------------
	//## validated
	
	object Validated {
		def genFile(outDir:File):File	= {
			val outFile	= outDir / "ValidatedGenerated.scala"
			IO write (outFile,	getTrait)
			outFile
		}
		
		def getTrait:String	= {
			s"""
			package scutil.lang
			
			import scutil.lang.tc._
			
			trait ValidatedGenerated {
				//------------------------------------------------------------------------------
				// zip
				${3 to 22 map genZip	mkString "\n"}
				//------------------------------------------------------------------------------
				// lift
				${2 to 22 map genLift	mkString "\n"}
			}
			"""
		}
		
		def genZip(arity:Int):String	= {
			val idxs	= 1 to arity
			s"""
			def zip${arity}
				[E:CanConcat,${csep(idxs map plainType)}]
				(${(csep(idxs map wrapInput))})
				:
				Validated[E,(${csep(idxs map plainType)})]
				=
				${idxs map argValue mkString " zip "} map Tuples.runcurry${arity}
			"""
		}
		
		def genLift(arity:Int):String	= {
			val idxs	= 1 to arity
			s"""
			def lift${arity}
				[E:CanConcat,${csep(idxs map plainType)},T]
				(func:(${csep(idxs map plainType)})=>T)
				:
				(${csep(idxs map wrapType)})=>Validated[E,T]
				=
				(${csep(idxs map wrapInput)})
				=>
				Validated good func.curried ap ${idxs map argValue mkString " ap "}
			"""
		}
		
		def plainType(idx:Int):String	= "S" + idx.toString
		def wrapType(idx:Int):String	= s"Validated[E,${plainType(idx)}]"
		def argValue(idx:Int):String	= "s" + idx.toString
		def plainInput(idx:Int):String	= argValue(idx) + ":" + plainType(idx)
		def wrapInput(idx:Int):String	= argValue(idx) + ":" + wrapType(idx)
		def csep(s:Seq[String]):String	= s mkString ","
	}

	//------------------------------------------------------------------------------
	//## tuples
	
	object Tuples {
		def genFile(outDir:File):File	= {
			val outFile	= outDir / "TuplesGenerated.scala"
			IO write (outFile,	getTrait)
			outFile
		}
		
		def getTrait:String	= {
			s"""
			package scutil.lang
			
			trait TuplesGenerated {
				${2 to 22 map genMethods mkString "\n"}
			}
			"""
		}
		
		def genMethods(arity:Int):String	= {
			val tpForward	= Types(1 to arity)
			val tpReverse	= tpForward.reverse
			
			def typeName(index:Int):String	= "T" + index
			def inName(index:Int):String	= "in._" + index
			
			val tpNameForward	= tpForward map typeName
			val tpInForward		= tpForward map inName
			val tpNameReverse	= tpReverse map typeName
			val tpInReverse		= tpReverse map inName
			
			val leftType	= "type " + leftCurriedType(tpNameForward)	+ " = " + pairRight(tpNameForward)
			val rightType	= "type " + rightCurriedType(tpNameForward)	+ " = " + pairLeft(tpNameForward)
		
			val leftCurry	=
					methodHeader("lcurry", tpNameForward)		+ "\n" +
					"(in:" + productType(tpNameForward) + "):"	+ "\n" +
					leftCurriedType(tpNameForward)				+ "\n" +
					" = "										+ "\n" +
					(
						tpInForward.right.fold[String](
							(t1,t2)	=> "(" + t1 + "," + t2 + ")",
							(tt,xx)	=> "(" + tt + "," + xx + ")"
						)
					)
			val rightCurry	=
					methodHeader("rcurry", tpNameForward)		+ "\n" +
					"(in:" + productType(tpNameForward) + "):"	+ "\n" +
					rightCurriedType(tpNameForward)				+ "\n" +
					" = "										+ "\n" +
					(
						tpInForward.left.fold[String](
							(t1,t2)	=> "(" + t1 + "," + t2 + ")",
							(xx,t)	=> "(" + xx + "," + t + ")"
						)
					)
					
			val leftUncurry	=
					methodHeader("luncurry", tpNameForward)			+ "\n" +
					"(in:" + leftCurriedType(tpNameForward) + "):"	+ "\n" +
					tupleType(tpNameForward)						+ "\n" +
					" = "											+ "\n" +
					{
						val (_,s)	= tpForward.right.fold[(Int,String)](
							(t1:Int,t2:Int)	=> {
								val depth	= tpForward.size-2
								val prefix	= "._2" * depth
								(depth-1, "in" + prefix + "._1" + "," + "in" + prefix + "._2")
							},
							{ case (tt, xx)	=>
								val (depth,xs)	= xx
								val prefix		= "._2" * depth
								(depth-1, "in" + prefix + "._1" + "," + xs)
							}
						)
						"(" + s + ")"
					}
			val rightUncurry	=
					methodHeader("runcurry", tpNameForward)			+ "\n" +
					"(in:" + rightCurriedType(tpNameForward) + "):"	+ "\n" +
					tupleType(tpNameForward)						+ "\n" +
					" = "											+ "\n" +
					{
						val (_,s)	= tpForward.left.fold[(Int,String)](
							(t1:Int,t2:Int)	=> {
								val depth	= tpForward.size-2
								val prefix	= "._1" * depth
								(depth-1, "in" + prefix + "._1" + "," + "in" + prefix + "._2")
							},
							{ case (xx, tt)	=>
								val (depth,xs)	= xx
								val prefix	= "._1" * depth
								(depth-1, xs + "," + "in" + prefix + "._2")
							}
						)
						"(" + s + ")"
					}
					
			val leftFlip	=
					methodHeader("lflip", tpNameForward)			+ "\n" +
					"(in:" + leftCurriedType(tpNameForward) + "):"	+ "\n" +
					rightCurriedType(tpNameForward)					+ "\n" +
					" = " 											+ "\n" +
					methodName("rcurry", tpNameForward) 	+ "(" +
					methodName("luncurry", tpNameForward)	+ "(" +
					"in" + "))"
			val rightFlip	=
					methodHeader("rflip", tpNameForward)			+ "\n" +
					"(in:" + rightCurriedType(tpNameForward) + "):" + "\n" +
					leftCurriedType(tpNameForward)					+ "\n" +
					" = "											+ "\n" +
					methodName("lcurry", tpNameForward) 	+ "(" +
					methodName("runcurry", tpNameForward)	+ "(" +
					"in" + "))"
					
			val reverse	=
					methodHeader("reverse", tpNameForward)		+ "\n" +
					"(in:" + productType(tpNameForward) + "):"	+ "\n" +
					tupleType(tpNameReverse)					+ "\n" +
					" = "										+ "\n" +
					(tpInReverse.linear mkString ("(", ",", ")"))
					
			s"""
			//------------------------------------------------------------------------------
			// arity ${arity}
			
			${leftType}
			${rightType}
			
			${leftCurry}
			${rightCurry}
			
			${leftUncurry}
			${rightUncurry}
			
			${leftFlip}
			${rightFlip}
			
			${reverse}
			"""
		}
		
		def methodHeader(name:String, in:Types[String]):String	=
				"def " + methodName(name, in) + paramsLinear(in)
			
		def methodName(name:String, in:Types[String]):String	=
				name + in.size
			
		def leftCurriedType(in:Types[String]):String	=
				"LCurried" + in.size + paramsLinear(in)
			
		def rightCurriedType(in:Types[String]):String	=
				"RCurried" + in.size + paramsLinear(in)
			
		def productType(in:Types[String]):String	=
				"Product" + in.size + paramsLinear(in)
			
		def tupleType(in:Types[String]):String	=
				"Tuple" + in.size + paramsLinear(in)
			
		def paramsLinear(in:Types[String]):String	=
				in.linear mkString ("[", ",", "]")
	
		def pairLeft(in:Types[String]):String	=
				in.left.fold[String](
					(t1,t2)	=> "(" + t1 + "," + t2 + ")",
					(xx,tt)	=> "(" + xx + "," + tt + ")"
				)
				
		def pairRight(in:Types[String]):String	=
				in.right.fold[String](
					(t1,t2)	=> "(" + t1 + "," + t2 + ")",
					(tt,xx)	=> "(" + tt + "," + xx + ")"
				)
		
		//==============================================================================
		
		case class Types[T](linear:Seq[T]) {
			def size	= linear.size
			def left	= lTree(linear)
			def right	= rTree(linear)
			
			def reverse:Types[T]			= Types(linear.reverse)
			def map[U](func:T=>U):Types[U]	= Types(linear map func)
		}
		
		//------------------------------------------------------------------------------
		
		def lTree[T](in:Seq[T]):LTree[T]	= {
			def loop[T](in:List[T]):LTree[T]	= in match {
				case Nil			=> sys error "too short"
				case a :: Nil		=> sys error "too short"
				case a :: b :: Nil	=> LEnd(b,a)
				case a :: rest		=> LMid(loop(rest), a)
			}
			loop(in.reverse.toList)
		}
		
		sealed trait LTree[T] {
			def cata[X](end:(T,T)=>X, mid:(LTree[T],T)=>X):X	=
					this match {
						case LEnd(l,r)	=> end(l,r)
						case LMid(l,r)	=> mid(l, r)
					}
			def fold[X](end:(T,T)=>X, mid:(X,T)=>X):X	=
					cata(
						end,
						(x,t) => mid(x fold(end, mid), t)
					)
			def map[U](func:T=>U):LTree[U]	=
					cata(
						(l,r)	=> LEnd(func(l), func(r)),
						(l,r)	=> LMid(l map func, func(r))
					)
		}
		case class LMid[T](left:LTree[T], right:T)	extends LTree[T]
		case class LEnd[T](left:T, right:T)			extends LTree[T]
		
		//------------------------------------------------------------------------------
		
		def rTree[T](in:Seq[T]):RTree[T]	= {
			def loop[T](in:List[T]):RTree[T]	= in match {
				case Nil			=> sys error "too short"
				case a :: Nil		=> sys error "too short"
				case a :: b :: Nil	=> REnd(a,b)
				case a :: rest		=> RMid(a, loop(rest))
			}
			loop(in.toList)
		}
		
		sealed trait RTree[T] {
			def cata[X](end:(T,T)=>X, mid:(T,RTree[T])=>X):X	=
					this match {
						case REnd(l,r)	=> end(l,r)
						case RMid(l,r)	=> mid(l, r)
					}
			def fold[X](end:(T,T)=>X, mid:(T,X)=>X):X	=
					cata(
						end,
						(t,x) => mid(t, x fold(end,mid))
					)
			def map[U](func:T=>U):RTree[U]	=
					cata(
						(l,r)	=> REnd(func(l), func(r)),
						(l,r)	=> RMid(func(l), r map func)
					)
		}
		case class RMid[T](left:T, right:RTree[T])	extends RTree[T]
		case class REnd[T](left:T, right:T)			extends RTree[T]
	}
}
