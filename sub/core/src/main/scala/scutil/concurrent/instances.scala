package scutil.concurrent

import java.util.concurrent.locks.Lock
import java.util.TimerTask

import scutil.lang.tc._

object instances extends instances
trait instances {
	implicit def LockResource[T<:Lock]:Resource[T]				= Resource by (_.unlock())
	implicit def TimerTaskResource[T<:TimerTask]:Resource[T]	= Resource by (_.cancel())
}
