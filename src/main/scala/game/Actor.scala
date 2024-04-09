package game
import scala.math.*

sealed trait Actor:

  val canBeMovedThrough: Boolean
  
  def getAgility: Int
  
  def move(to: Square, alongPath: Vector[Square]): Unit

end Actor

case class Obstacle() extends Actor:

  val canBeMovedThrough = false
  
  def getAgility = 0
  
  def move(to: Square, alongPath: Vector[Square]) = ()

case class Character(val battle: Battle, val name: String, startHP: Int, private val armor: Int, private val agility: Int,  private val abilities: Vector[Ability]) extends Actor:

  val canBeMovedThrough: Boolean = false
  
  private var square: Option[Square] = None
  private val maxHP = startHP
  private var hp = startHP
  private val status: Option[Status] = None
  private var turnAgility = this.agility
  
  def move(to: Square, alongPath: Vector[Square]) =
    if this.square.isDefined then
      this.square.head.removeActor(this)
    to.addActor(this)
    square = Some(to)
    turnAgility = max(0, turnAgility - alongPath.length)
    
  def location = square
  
  def getAgility = this.turnAgility

  def isStuck = this.turnAgility == 0
  
  def getStatus = status
  
  def maxHealth = this.maxHP
  
  def currentHP = this.hp

  def takeDamage(amount: Int) =
    hp = max(hp - max(0, amount - armor), 0)

  def healDamage(amount: Int) =
    hp = min(hp + amount, maxHP)

  def isDown = hp <= 0
  
  def getAbilities = abilities