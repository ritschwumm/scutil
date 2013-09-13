package scutil.lang

import scutil.lens._

object Subtype {
	def apply[Super,Sub<:Super](downcast:Super=>Option[Sub]):Subtype[Super,Sub]	=
			new FunctionSubtype(downcast)
		
	def partial[Super,Sub<:Super](pfunc:PartialFunction[Super,Sub]):Subtype[Super,Sub]	=
			Subtype(pfunc.lift)
}

trait Subtype[Super,Sub<:Super] {
	def downcast(it:Super):Option[Sub]
	def upcast(it:Sub):Super	= it
	
	// can be used as scala function and extractor
	def apply(it:Sub):Super				= upcast(it)
	def unapply(it:Super):Option[Sub]	= downcast(it)
	
	def compose[Top>:Super](that:Subtype[Top,Super]):Subtype[Top,Sub]	=
			Subtype(that downcast _ flatMap this.downcast)
		
	def andThen[Bottom<:Sub](that:Subtype[Sub,Bottom]):Subtype[Super,Bottom]	=
			that compose this
			
	def asMarshaller:Marshaller[Sub,Super]	= Marshaller(upcast, downcast)
	def asPLens:PLens[Super,Sub]			= PLens(downcast(_) map (Store(_, upcast)))
	
	def downcastExtractor:Extractor[Super,Sub]	= Extractor(downcast)
	def upcastFunction:Function[Sub,Super]		= upcast _
}

final private class FunctionSubtype[Super,Sub<:Super](downcastFunc:Super=>Option[Sub]) extends Subtype[Super,Sub] {
	def downcast(it:Super):Option[Sub]	= downcastFunc(it)
}
