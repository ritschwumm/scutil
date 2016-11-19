package scutil.concurrent

import java.util.concurrent.locks.Lock
import java.util.TimerTask

import scutil.lang._

object disposables extends disposables
trait disposables {
	implicit def DisposableForLock(peer:Lock):Disposable		= Disposable(peer.unlock _)
	implicit def DisposableTimerTask(peer:TimerTask):Disposable	= Disposable(peer.cancel _)
}
