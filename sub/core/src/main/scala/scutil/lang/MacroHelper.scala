package scutil.lang

import scala.reflect.macros.Context

private trait MacroHelper {
	val c:Context
	import c.universe._
	
	//------------------------------------------------------------------------------
	
	def mkParam(name:TermName, tpe:Type):ValDef	=
			ValDef(
				Modifiers(Flag.PARAM),
				name, 
				TypeTree(tpe),
				EmptyTree
			)
			
	def mkAccess(a:TermName, b:TermName):Select	=
			Select(
				Ident(a), 
				b
			)
			
	//------------------------------------------------------------------------------
	
	def stringIdent(s:String):Ident	=
			Ident(newTermName(s))
		
	// NOTE Strings are implicitly converted to TermName
	def multiSelect(start:TermName, names:String*):Tree	=
			multiSelect(Ident(start), names:_*)
		
	def multiSelect(start:Symbol, names:String*):Tree	=
			multiSelect(Ident(start), names:_*)     
		
	def multiSelect(start:Tree, names:String*):Tree	=
			(names foldLeft start) { (last:Tree, name:String) => 
				Select(last, newTermName(name)) 
			}
			
	//------------------------------------------------------------------------------
	
	def result[T](out:Tried[String,Tree]):c.Expr[T]	=
			out cata (
				it	=> c abort (c.enclosingPosition, it),
				it	=> c.Expr[T](c resetAllAttrs it)
			)
}
