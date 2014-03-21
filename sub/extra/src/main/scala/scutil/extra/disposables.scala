package scutil.extra

import java.sql.Connection
import java.sql.ResultSet
import java.sql.Statement
import java.beans.XMLEncoder
import java.beans.XMLDecoder

import scutil.lang._

object disposables extends disposables 
trait disposables {
	implicit def DisposableForConnection		(peer:Connection)		= Disposable(peer.close)
	implicit def DisposableForResultSet			(peer:ResultSet)		= Disposable(peer.close)
	implicit def DisposableForStatement			(peer:Statement)		= Disposable(peer.close)
	
	implicit def DisposableForXMLDecoder		(peer:XMLDecoder)		= Disposable(peer.close)
	implicit def DisposableForXMLEncoder		(peer:XMLEncoder)		= Disposable(peer.close)
}
