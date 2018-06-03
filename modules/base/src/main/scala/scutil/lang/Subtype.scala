package scutil.lang

import scala.reflect.ClassTag

object Subtype {
	@deprecated("use Prism", "0.139.0")
	def partial[Super,Sub<:Super](pfunc:PartialFunction[Super,Sub]):Subtype[Super,Sub]	=
			Subtype(pfunc.lift)
		
	@deprecated("use Prism", "0.139.0")
	def identity[T]:Subtype[T,T]	=
			Subtype(Some.apply)
		
	/** throwables have no type parameters so variance is not an issue here */
	@deprecated("use Prism", "0.139.0")
	def throwable[E<:Throwable:ClassTag]:Subtype[Throwable,E]	=
			Subtype partial {
				case x:E	=> x
			}
}

@deprecated("use Prism", "0.139.0")
final case class Subtype[Super,Sub<:Super](downcast:PFunction[Super,Sub]) {
	@deprecated("use Prism", "0.139.0")
	val upcast:Sub=>Super	= identity
	
	// can be used as scala function and extractor
	@deprecated("use Prism", "0.139.0")
	def apply(it:Sub):Super				= upcast(it)
	@deprecated("use Prism", "0.139.0")
	def unapply(it:Super):Option[Sub]	= downcast(it)
	
	/** symbolic alias for andThen */
	@deprecated("use Prism", "0.139.0")
	def >=>[Bottom<:Sub](that:Subtype[Sub,Bottom]):Subtype[Super,Bottom]	=
			this andThen that
		
	/** symbolic alias for compose */
	@deprecated("use Prism", "0.139.0")
	def <=<[Top>:Super](that:Subtype[Top,Super]):Subtype[Top,Sub]	=
			this compose that
	
	@deprecated("use Prism", "0.139.0")
	def compose[Top>:Super](that:Subtype[Top,Super]):Subtype[Top,Sub]	=
			Subtype(that downcast _ flatMap this.downcast)
		
	@deprecated("use Prism", "0.139.0")
	def andThen[Bottom<:Sub](that:Subtype[Sub,Bottom]):Subtype[Super,Bottom]	=
			that compose this
			
	@deprecated("use Prism", "0.139.0")
	def toPrism:Prism[Super,Sub]	=
			Prism(downcast, upcast)
		
	@deprecated("use Prism", "0.139.0")
	def toOptional:Optional[Super,Sub]	=
			toPrism.toOptional
		
	@deprecated("use Prism", "0.139.0")
	def toPLens:PLens[Super,Sub]	=
			PLens(downcast(_) map (Store(_, upcast)))
		
	@deprecated("use Prism", "0.139.0")
	def toPBijection:PBijection[Super,Sub]	=
			toPrism.toPBijection
	
	@deprecated("use Prism", "0.139.0")
	def downcastExtractor:Extractor[Super,Sub]	=
			Extractor(downcast)
}
