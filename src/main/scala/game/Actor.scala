package game
import scala.math.*

sealed trait Actor:

  val canBeMovedThrough: Boolean

end Actor

case class Obstacle() extends Actor:

  val canBeMovedThrough = false

case class Character(val name: String, startHP: Int, private val agility: Int) extends Actor:

  val canBeMovedThrough: Boolean = false
  private val maxHP = startHP
  private var hp = startHP
  
  def takeDamage(amount: Int) =
    hp = max(hp - amount, 0)
  
  def healDamage(amount: Int) =
    hp = min(hp + amount, maxHP)
    
  def isDown = hp <= 0