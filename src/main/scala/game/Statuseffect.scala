package game

sealed trait Statuseffect(var duration: Int):

  val name: String
  val isNegative: Boolean

  def increaseDuration(numberOfTurns: Int) = this.duration += numberOfTurns

  def tickTimer() = duration -= 1

  def applyEffect(target: Actor): Unit

  override def toString: String = this.name

end Statuseffect

class Burn(dur: Int) extends Statuseffect(dur):

  val name = "Burn"
  val isNegative = true

  def applyEffect(target: Actor): Unit =
    if this.duration <= 0 then
      target.removeStatus(this)
    else
      target.armorChange = -15
      tickTimer()

class Bleeding(dur: Int) extends Statuseffect(dur):

  val name = "Bleeding"
  val isNegative = true

  def applyEffect(target: Actor): Unit =
    if this.duration <= 0 then
      target.removeStatus(this)
    else
      target.takeDamage(40)
      tickTimer()