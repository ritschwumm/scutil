package scutil

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

object Resources {
	implicit def DisposableResource			[T <: Disposable]		(delegate:T)	= new Resource(delegate, delegate.dispose)
	
	implicit def CloseableResource			[T <: Closeable]		(delegate:T)	= new Resource(delegate, delegate.close)
	
	implicit def SocketResource				[T <: Socket]			(delegate:T)	= new Resource(delegate, delegate.close)
	implicit def ServerSocketResource		[T <: ServerSocket]		(delegate:T)	= new Resource(delegate, delegate.close)
	implicit def DatagramSocketResource		[T <: DatagramSocket]	(delegate:T)	= new Resource(delegate, delegate.close)
	
	implicit def ChannelResource			[T <: Channel]			(delegate:T)	= new Resource(delegate, delegate.close)
	implicit def SelectorResource			[T <: Selector]			(delegate:T)	= new Resource(delegate, delegate.close)
	implicit def FileLockResource			[T <: FileLock]			(delegate:T)	= new Resource(delegate, delegate.release)

	implicit def ConnectionResource			[T <: Connection]		(delegate:T)	= new Resource(delegate, delegate.close)
	implicit def ResultSetResource			[T <: ResultSet]		(delegate:T)	= new Resource(delegate, delegate.close)
	implicit def StatementResource			[T <: Statement]		(delegate:T)	= new Resource(delegate, delegate.close)
	
	implicit def LockResource				[T <: Lock]				(delegate:T)	= new Resource(delegate, delegate.unlock)
	
	// implicit def XMLDecoderResource		[T <: XMLDecoder]		(delegate:T)	= new Resource(delegate, delegate.close)
	// implicit def XMLEncoderResource		[T <: XMLEncoder]		(delegate:T)	= new Resource(delegate, delegate.close)
	implicit def ScannerResource			[T <: Scanner]			(delegate:T)	= new Resource(delegate, delegate.close)
	implicit def ImageInputStreamResource	[T <: ImageInputStream]	(delegate:T)	= new Resource(delegate, delegate.close)
	implicit def GraphicsResource			[T <: Graphics]			(delegate:T)	= new Resource(delegate, delegate.dispose)
}
