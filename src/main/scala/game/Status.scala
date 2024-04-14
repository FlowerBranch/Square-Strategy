package game


sealed trait StatusEffect(var duration: Int):

  val name: String
  val isNegative: Boolean
  
  def increaseDuration(numberOfTurns: Int) = this.duration += numberOfTurns

  def applyEffect(target: Actor): Unit

  override def toString: String = this.name

end StatusEffect

class Burn(duration: Int) extends StatusEffect(duration):

  val name = "Burn"
  val isNegative = true

  def applyEffect(target: Actor): Unit =
    target.armorChange = -15

class Bleeding(duration: Int) extends StatusEffect(duration):

  val name = "Bleeding"
  val isNegative = true

  def applyEffect(target: Actor): Unit =
    target.takeDamage(20)