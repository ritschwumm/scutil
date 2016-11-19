package scutil.lang

object disposables extends disposables 
trait disposables {
	implicit def DisposableForAutoCloseable(peer:AutoCloseable):Disposable	= Disposable(peer.close _)
}
