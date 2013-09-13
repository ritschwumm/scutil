package scutil.pimp

import java.io._
import java.net._
import java.sql._
import java.nio.channels._
import java.util.concurrent.locks.Lock
import java.util.Scanner
import javax.imageio.stream.ImageInputStream
import java.awt.Graphics
// import java.beans.XMLEncoder
// import java.beans.XMLDecoder

import scutil.lang._

object DisposableConversions extends DisposableConversions 
	
trait DisposableConversions {
	implicit def DisposableForCloseable			(delegate:Closeable)		= Disposable(delegate.close)
	
	implicit def DisposableForSocket			(delegate:Socket)			= Disposable(delegate.close)
	implicit def DisposableForServerSocket		(delegate:ServerSocket)		= Disposable(delegate.close)
	implicit def DisposableForDatagramSocket	(delegate:DatagramSocket)	= Disposable(delegate.close)
	
	implicit def DisposableForChannel			(delegate:Channel)			= Disposable(delegate.close)
	implicit def DisposableForSelector			(delegate:Selector)			= Disposable(delegate.close)
	implicit def DisposableForFileLock			(delegate:FileLock)			= Disposable(delegate.release)

	implicit def DisposableForConnection		(delegate:Connection)		= Disposable(delegate.close)
	implicit def DisposableForResultSet			(delegate:ResultSet)		= Disposable(delegate.close)
	implicit def DisposableForStatement			(delegate:Statement)		= Disposable(delegate.close)
	
	implicit def DisposableForLock				(delegate:Lock)				= Disposable(delegate.unlock)
	
	// implicit def DisposableForXMLDecoder		(delegate:XMLDecoder)		= Disposable(delegate.close)
	// implicit def DisposableForXMLEncoder		(delegate:XMLEncoder)		= Disposable(delegate.close)
	implicit def DisposableForScanner			(delegate:Scanner)			= Disposable(delegate.close)
	implicit def DisposableForImageInputStream	(delegate:ImageInputStream)	= Disposable(delegate.close)
	implicit def DisposableForGraphics			(delegate:Graphics)			= Disposable(delegate.dispose)
}
