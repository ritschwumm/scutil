package scutil.io

import java.io.Closeable
import java.net.Socket
import java.net.ServerSocket
import java.net.DatagramSocket
import java.nio.channels.Channel
import java.nio.channels.Selector
import java.nio.channels.FileLock

import scutil.lang._

object disposables extends disposables 
trait disposables {
	implicit def DisposableForCloseable			(peer:Closeable)		= Disposable(peer.close)
	
	implicit def DisposableForSocket			(peer:Socket)			= Disposable(peer.close)
	implicit def DisposableForServerSocket		(peer:ServerSocket)		= Disposable(peer.close)
	implicit def DisposableForDatagramSocket	(peer:DatagramSocket)	= Disposable(peer.close)
	
	implicit def DisposableForChannel			(peer:Channel)			= Disposable(peer.close)
	implicit def DisposableForSelector			(peer:Selector)			= Disposable(peer.close)
	implicit def DisposableForFileLock			(peer:FileLock)			= Disposable(peer.release)
}
