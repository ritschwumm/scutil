package scutil.uid

final case class Uid(
	machine:Long,
	counter:Long,
	time:Long,
	random:Long
)
