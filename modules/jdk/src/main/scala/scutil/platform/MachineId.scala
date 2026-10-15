package scutil.platform

import java.lang.{ Integer as JInteger, Long as JLong }
import java.lang.management.ManagementFactory
import java.net.NetworkInterface

import scutil.lang.*
import scutil.core.implicits.*
import scutil.jdk.implicits.*

object MachineId {
	lazy val integer:Int	=
		hashBytes.foldLeft(0)	{ (out, byte) => JInteger.rotateLeft(out, 8) ^ (byte & 0xff) }

	lazy val long:Long	=
		hashBytes.foldLeft(0L)	{ (out, byte) => JLong.rotateLeft(out, 8) ^ (byte & 0xff) }

	/** provides a per-machine hash similar to how mongodb works */
	val hashBytes:Vector[Byte]	= {
		val ifaces:Vector[NetworkInterface]	= NetworkInterface.getNetworkInterfaces.toIterator.toVector

		val ifaceMacs:Vector[Byte]	=
			for {
				iface	<- ifaces
				addr	<- iface.getHardwareAddressOption.toVector
				byte	<- addr.toVector
			}
			yield byte

		val ifaceNames:Vector[Byte]	=
			for {
				iface	<- ifaces
				name	<- iface.getDisplayNameOption.toVector
				byte	<- name.getBytes("utf-8").toVector
			}
			yield byte

		val process:Vector[Byte]	=
			for {
				name	<- Catch.exception.in(ManagementFactory.getRuntimeMXBean.getName).toVector
				byte	<- name.getBytes("utf-8").toVector
			}
			yield byte

		val loader:Vector[Byte]	=
			System.identityHashCode(getClass.getClassLoader).toString.getBytes("utf-8").toVector

		ifaceMacs ++ ifaceNames ++ process ++ loader
	}
}
