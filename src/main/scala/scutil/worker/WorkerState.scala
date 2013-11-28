package scutil.worker

import scutil.time.MilliDuration

sealed trait WorkerState

case object WorkerWaiting						extends WorkerState
case object WorkerWorking						extends WorkerState
case class WorkerSleeping(left:MilliDuration)	extends WorkerState
case object WorkerDead							extends WorkerState
