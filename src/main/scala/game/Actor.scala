package game
import scala.math.*

sealed trait Actor:

  val canBeMovedThrough: Boolean
  var onTheMove: Option[Vector[Square]]
  
  def getAgility: Int
  
  def move(to: Square, alongPath: Vector[Square]): Unit

end Actor

case class Obstacle() extends Actor:

  val canBeMovedThrough = false
  var onTheMove: Option[Vector[Square]] = None
  def getAgility = 0
  
  def move(to: Square, alongPath: Vector[Square]) = ()

case class Character(val battle: Battle, val name: String, startHP: Int, private val armor: Int, private val agility: Int,  private val abilities: Vector[Ability]) extends Actor:

  val canBeMovedThrough: Boolean = false
  var turnEnded = false
  var abilityUsed = false
  var onTheMove: Option[Vector[Square]] = None
  private var square: Option[Square] = None
  private val maxHP = startHP
  private var hp = startHP
  private val status: Option[Status] = None
  private var turnAgility = this.agility
  
  def turnStartState() =
    this.turnAgility = this.agility
    abilityUsed = false
    turnEnded = false
  
  def move(to: Square, alongPath: Vector[Square]) =
    if this.square.isDefined then
      this.square.head.removeActor(this)
    to.addActor(this)
    square = Some(to)
    turnAgility = max(0, turnAgility - alongPath.length)
    
  def followPath() =
    if onTheMove.isDefined then
      if onTheMove.head.nonEmpty then
        this.move(onTheMove.head.head, Vector(onTheMove.head.head))
        onTheMove = Some(onTheMove.head.tail)
      else
        onTheMove = None
    else
      ()
      
  def location = square
  
  def getAgility = this.turnAgility

  def isStuck = this.turnAgility == 0
  
  def turnIsOver = this.turnEnded || this.isDown || (this.isStuck && this.abilityUsed)
  
  def getStatus = status
  
  def maxHealth = this.maxHP
  
  def currentHP = this.hp

  def takeDamage(amount: Int) =
    hp = max(hp - max(0, amount - armor), 0)

  def healDamage(amount: Int) =
    hp = min(hp + amount, maxHP)

  def isDown = hp <= 0
  
  def getAbilities = abilities