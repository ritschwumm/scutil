package scutil.text

import java.util.Scanner

import scutil.lang._

object disposables extends disposables
trait disposables {
	implicit def DisposableForScanner	(peer:Scanner)	= Disposable(peer.close)
}
