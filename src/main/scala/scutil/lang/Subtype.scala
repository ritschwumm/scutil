package scutil.lang

import scutil.lens._

object Subtype {
	def partial[Super,Sub<:Super](pfunc:PartialFunction[Super,Sub]):Subtype[Super,Sub]	=
			Subtype(pfunc.lift)
}

final case class Subtype[Super,Sub<:Super](downcast:PFunction[Super,Sub]) {
	val upcast:Sub=>Super	= identity
	
	// can be used as scala function and extractor
	def apply(it:Sub):Super				= upcast(it)
	def unapply(it:Super):Option[Sub]	= downcast(it)
	
	/** symbolic alias for andThen */
	@inline
	def >=>[Bottom<:Sub](that:Subtype[Sub,Bottom]):Subtype[Super,Bottom]	=
			this andThen that
		
	/** symbolic alias for compose */
	@inline
	def <=<[Top>:Super](that:Subtype[Top,Super]):Subtype[Top,Sub]	=
			this compose that
	
	def compose[Top>:Super](that:Subtype[Top,Super]):Subtype[Top,Sub]	=
			Subtype(that downcast _ flatMap this.downcast)
		
	def andThen[Bottom<:Sub](that:Subtype[Sub,Bottom]):Subtype[Super,Bottom]	=
			that compose this
			
	def asMarshaller:Marshaller[Sub,Super]	=
			Marshaller(upcast, downcast)
		
	def asPLens:PLens[Super,Sub]	= 
			PLens(downcast(_) map (Store(_, upcast)))
	
	def downcastExtractor:Extractor[Super,Sub]	=
			Extractor(downcast)
}
