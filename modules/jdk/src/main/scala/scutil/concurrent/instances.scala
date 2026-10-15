package scutil.concurrent

import scala.annotation.targetName

import java.util.concurrent.locks.Lock
import java.util.TimerTask

import scala.util.Using.Releasable

object instances {
	@targetName("LockReleasable")
	given [T<:Lock] => Releasable[T]		= _.unlock()

	@targetName("TimerTaskReleasable")
	given [T<:TimerTask] => Releasable[T]	= _.cancel()
}
