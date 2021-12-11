package scutil.concurrent

import java.util.concurrent.locks.Lock
import java.util.TimerTask

import scala.util.Using.Releasable

object instances {
	given LockReleasable[T<:Lock]:Releasable[T]				= _.unlock()
	given TimerTaskReleasable[T<:TimerTask]:Releasable[T]	= _.cancel()
}
