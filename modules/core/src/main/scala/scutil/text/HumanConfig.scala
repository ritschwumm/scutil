package scutil.text

object HumanConfig {
	val default	=
		HumanConfig(
			maxUnits		= 10000,
			smallUnits		= 10000,
			decimalPlaces	= 0
		)
}

final case class HumanConfig(
	maxUnits:Int,
	smallUnits:Int,
	decimalPlaces:Int,
)
