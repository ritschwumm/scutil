package scutil.uid

@deprecated("use scutil.guid.Guid", "0.144.0")
final case class Uid(
	machine:Long,
	counter:Long,
	time:Long,
	random:Long
)
