package scutil.worker

object WorkerCommand {
	case object Start	extends WorkerCommand
	case object Stop	extends WorkerCommand
	case object Die		extends WorkerCommand
}

sealed trait WorkerCommand

