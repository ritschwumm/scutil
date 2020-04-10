package scutil.lang.tc

object ShowSyntax extends ShowSyntax

trait ShowSyntax {
	implicit final class ShowValueSyntaxExt[T](peer:T)(implicit TC:Show[T]) {
		def show:String	= TC show peer
	}
}
