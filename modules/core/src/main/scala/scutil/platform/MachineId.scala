package scutil.platform

import java.lang.{ Integer => JInteger, Long => JLong }
import java.lang.management.ManagementFactory
import java.net.NetworkInterface

import scutil.lang._
import scutil.base.implicits._
import scutil.core.implicits._

object MachineId {
	lazy val integer:Int	=
			(hashBytes foldLeft 0)	{ (out, byte) => (JInteger	rotateLeft (out, 8)) ^ (byte & 0xff) }
		
	lazy val long:Long	=
			(hashBytes foldLeft 0L)	{ (out, byte) => (JLong		rotateLeft (out, 8)) ^ (byte & 0xff) }
		
	/** provides a per-machine hash similar to how mongodb works */
	val hashBytes:ISeq[Byte]	= {
		val ifaces:Vector[NetworkInterface]	= NetworkInterface.getNetworkInterfaces.toIterator.toVector
		
		val ifaceMacs:ISeq[Byte]	=
				for {
					iface	<- ifaces
					addr	<- (iface.getHardwareAddress:Seq[Byte]).optionNotNull.toVector
					byte	<- addr
				}
				yield byte
				
		val ifaceNames:ISeq[Byte]	=
				for {
					iface	<- ifaces
					byte	<- iface.getDisplayName getBytes "utf-8"
				}
				yield byte
				
		val process:ISeq[Byte]	=
				for {
					name	<- (Catch.exception in ManagementFactory.getRuntimeMXBean.getName).toVector
					byte	<- name getBytes "utf-8"
				}
				yield byte
		
		val loader:ISeq[Byte]	=
				((System identityHashCode getClass.getClassLoader).toString getBytes "utf-8").toVector
			
		(ifaceMacs ++ ifaceNames ++ process ++ loader).toVector
	}
}
