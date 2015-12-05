package scutil.xml

import java.beans.XMLEncoder
import java.beans.XMLDecoder

import scutil.lang._

object disposables extends disposables
trait disposables {
	implicit def DisposableForXMLDecoder	(peer:XMLDecoder)	= Disposable(peer.close)
	implicit def DisposableForXMLEncoder	(peer:XMLEncoder)	= Disposable(peer.close)
}
