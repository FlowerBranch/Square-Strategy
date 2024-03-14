package game
import scala.math.*
import scala.collection.mutable.Buffer

sealed trait Actor:

  val canBeMovedThrough: Boolean

end Actor

case class Obstacle() extends Actor:

  val canBeMovedThrough = false

case class Character(val name: String, startHP: Int, private val armor: Int, private val agility: Int) extends Actor:

  val canBeMovedThrough: Boolean = false
  private val maxHP = startHP
  private var hp = startHP
  private val statuses = Buffer[Status]()
  
  def getAgility = this.agility

  def takeDamage(amount: Int) =
    hp = max(hp - max(0, amount - armor), 0)

  def healDamage(amount: Int) =
    hp = min(hp + amount, maxHP)

  def isDown = hp <= 0