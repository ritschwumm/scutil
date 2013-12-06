package scutil.platform

import java.lang.{ Integer => JInteger, Long => JLong }
import java.lang.management.ManagementFactory
import java.net.NetworkInterface

import scala.collection.JavaConverters._

import scutil.lang._
import scutil.implicits._

object MachineId {
	lazy val integer:Int	=
			(hashBytes foldLeft 0)	{ (out, byte) => (JInteger	rotateLeft (out, 8)) ^ (byte & 0xff) }
		
	lazy val long:Long	=
			(hashBytes foldLeft 0L)	{ (out, byte) => (JLong		rotateLeft (out, 8)) ^ (byte & 0xff) }
		
	/** provides a per-machine hash similar to how mongodb works */
	val hashBytes:Seq[Byte]	= {
		val ifaces:Vector[NetworkInterface]	= NetworkInterface.getNetworkInterfaces.asScala.toVector
		
		val ifaceMacs:Seq[Byte]	=
				for {
					iface		<- ifaces
					addr		<- (iface.getHardwareAddress:Seq[Byte]).guardNotNull.toVector
					byte		<- addr
				}
				yield byte
				
		val ifaceNames:Seq[Byte]	= 
				for {
					iface	<- ifaces
					byte	<- iface.getDisplayName getBytes "utf-8"
				}
				yield byte
				
		val process:Seq[Byte]	=
				for {
					name	<- (Tried catchException ManagementFactory.getRuntimeMXBean.getName).toVector
					byte	<- name getBytes "utf-8"
				}
				yield byte
		
		val loader:Seq[Byte]	=
				(System identityHashCode getClass.getClassLoader).toString getBytes "utf-8"
			
		(ifaceMacs ++ ifaceNames ++ process ++ loader).toVector
	}
}
