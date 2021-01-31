package scutil.concurrent

import java.util.concurrent.locks.Lock
import java.util.TimerTask

import scala.util.Using.Releasable

object instances extends instances

trait instances {
	implicit def LockReleasable[T<:Lock]:Releasable[T]				= _.unlock()
	implicit def TimerTaskReleasable[T<:TimerTask]:Releasable[T]	= _.cancel()
}
