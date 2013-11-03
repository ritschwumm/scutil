package scutil.pimp

import java.io._
import java.net._
import java.sql._
import java.nio.channels._
import java.util.concurrent.locks.Lock
import java.util.Scanner
import java.util.TimerTask
import javax.imageio.stream.ImageInputStream
import java.awt.Graphics
// import java.beans.XMLEncoder
// import java.beans.XMLDecoder

import scutil.lang._

object DisposableConversions extends DisposableConversions 
	
trait DisposableConversions {
	implicit def DisposableForCloseable			(peer:Closeable)		= Disposable(peer.close)
	
	implicit def DisposableForSocket			(peer:Socket)			= Disposable(peer.close)
	implicit def DisposableForServerSocket		(peer:ServerSocket)		= Disposable(peer.close)
	implicit def DisposableForDatagramSocket	(peer:DatagramSocket)	= Disposable(peer.close)
	
	implicit def DisposableForChannel			(peer:Channel)			= Disposable(peer.close)
	implicit def DisposableForSelector			(peer:Selector)			= Disposable(peer.close)
	implicit def DisposableForFileLock			(peer:FileLock)			= Disposable(peer.release)

	implicit def DisposableForConnection		(peer:Connection)		= Disposable(peer.close)
	implicit def DisposableForResultSet			(peer:ResultSet)		= Disposable(peer.close)
	implicit def DisposableForStatement			(peer:Statement)		= Disposable(peer.close)
	
	implicit def DisposableForLock				(peer:Lock)				= Disposable(peer.unlock)
	
	// implicit def DisposableForXMLDecoder		(peer:XMLDecoder)		= Disposable(peer.close)
	// implicit def DisposableForXMLEncoder		(peer:XMLEncoder)		= Disposable(peer.close)
	implicit def DisposableForScanner			(peer:Scanner)			= Disposable(peer.close)
	implicit def DisposableForImageInputStream	(peer:ImageInputStream)	= Disposable(peer.close)
	implicit def DisposableForGraphics			(peer:Graphics)			= Disposable(peer.dispose)
	implicit def DisposableTimerTask			(peer:TimerTask)		= Disposable(peer.cancel)
}
