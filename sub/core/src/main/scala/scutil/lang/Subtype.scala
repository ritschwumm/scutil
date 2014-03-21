package scutil.lang

import scala.reflect.ClassTag

object Subtype {
	def partial[Super,Sub<:Super](pfunc:PartialFunction[Super,Sub]):Subtype[Super,Sub]	=
			Subtype(pfunc.lift)
		
	/** throwables have no type parameters so variance is not an issue here */
	def throwable[E<:Throwable:ClassTag]:Subtype[Throwable,E]	=
			Subtype partial {
				case x:E	=> x
			}
}

final case class Subtype[Super,Sub<:Super](downcast:PFunction[Super,Sub]) {
	val upcast:Sub=>Super	= identity
	
	// can be used as scala function and extractor
	def apply(it:Sub):Super				= upcast(it)
	def unapply(it:Super):Option[Sub]	= downcast(it)
	
	/** symbolic alias for andThen */
	def >=>[Bottom<:Sub](that:Subtype[Sub,Bottom]):Subtype[Super,Bottom]	=
			this andThen that
		
	/** symbolic alias for compose */
	def <=<[Top>:Super](that:Subtype[Top,Super]):Subtype[Top,Sub]	=
			this compose that
	
	def compose[Top>:Super](that:Subtype[Top,Super]):Subtype[Top,Sub]	=
			Subtype(that downcast _ flatMap this.downcast)
		
	def andThen[Bottom<:Sub](that:Subtype[Sub,Bottom]):Subtype[Super,Bottom]	=
			that compose this
			
	def asPrism:Prism[Super,Sub]	=
			Prism(downcast, upcast)
		
	def asPLens:PLens[Super,Sub]	= 
			PLens(downcast(_) map (Store(_, upcast)))
		
	def asPBijection:PBijection[Super,Sub]	=
			asPrism.asPBijection
	
	def downcastExtractor:Extractor[Super,Sub]	=
			Extractor(downcast)
}
