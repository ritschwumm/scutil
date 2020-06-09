package scutil.worker

import scutil.time.MilliDuration

object WorkerState {
	case object	Waiting								extends WorkerState
	case object	Working								extends WorkerState
	final case class Sleeping(left:MilliDuration)	extends WorkerState
	case object	Dead								extends WorkerState
}

sealed trait WorkerState
