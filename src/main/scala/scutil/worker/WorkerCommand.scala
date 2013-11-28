package scutil.worker

sealed trait WorkerCommand

case object WorkerStart	extends WorkerCommand
case object WorkerStop	extends WorkerCommand
case object WorkerDie	extends WorkerCommand
