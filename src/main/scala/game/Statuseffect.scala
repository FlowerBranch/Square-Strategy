package game

/**
 * Statuseffect that affects character in different ways if obtained.
 * Duration of effect is given as a parameter
 */
sealed trait Statuseffect(var duration: Int):

  val name: String
  val isNegative: Boolean

  def increaseDuration(numberOfTurns: Int) = this.duration += numberOfTurns

  def tickTimer() = duration -= 1

  def applyEffect(target: Actor): Unit

  override def toString: String = this.name

end Statuseffect

/**
 * Status that decreases the amount of armor a character has
 * @param dur duration of status
 */
class Burn(dur: Int) extends Statuseffect(dur):

  val name = "Burn"
  val isNegative = true

  def applyEffect(target: Actor): Unit =
    if this.duration <= 0 then
      target.removeStatus(this)
    else
      target.armorChange = -15
      tickTimer()

/**
 * Status that makes a character take damage at turn start
 * @param dur duration of status
 */
class Bleeding(dur: Int) extends Statuseffect(dur):

  val name = "Bleeding"
  val isNegative = true

  def applyEffect(target: Actor): Unit =
    if this.duration <= 0 then
      target.removeStatus(this)
    else
      target.takeDamage(40)
      tickTimer()