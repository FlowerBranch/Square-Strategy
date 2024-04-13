package game
import scala.math.*

sealed trait Actor(val battle: Battle):

  val canBeMovedThrough: Boolean
  var onTheMove: Option[Vector[Square]]
  var currentSquare: Option[Square] = None
  
  def getAgility: Int
  
  def getStatuses: Vector[StatusEffect] 

  def move(to: Square, alongPath: Vector[Square]): Unit
  
  def applyStatus(statusEffect: Option[StatusEffect]): Unit
  
  def takeDamage(amount: Int): Unit
  
  def location: Option[Square] = this.currentSquare

end Actor

case class Obstacle(in: Battle) extends Actor(in):

  val canBeMovedThrough = false
  var onTheMove: Option[Vector[Square]] = None
  
  def getAgility = 0
  
  def getStatuses: Vector[StatusEffect] = Vector()
  
  def move(to: Square, alongPath: Vector[Square]) = ()
  
  def applyStatus(statusEffect: Option[StatusEffect]) = ()
  
  def takeDamage(amount: Int) = ()

case class Character(in: Battle,
                     val name: String,
                     startHP: Int,
                     private val armor: Int,
                     private val agility: Int,
                     private val abilities: Vector[Ability]
                    ) extends Actor(in):

  val canBeMovedThrough: Boolean = false
  var turnEnded = false
  var abilityUsed = false
  var onTheMove: Option[Vector[Square]] = None
  private val maxHP = startHP
  private var hp = startHP
  private var statuses = Vector[StatusEffect]()
  private var turnAgility = this.agility
  
  def turnStartState() =
    this.turnAgility = this.agility
    abilityUsed = false
    turnEnded = false
    
  def applyStatus(statusEffect: Option[StatusEffect]) =
    if statusEffect.isDefined then
      this.statuses = this.statuses ++ Vector(statusEffect.head)
    
  def getStatuses = this.statuses
  
  def move(to: Square, alongPath: Vector[Square]) =
    if this.currentSquare.isDefined then
      this.currentSquare.head.removeActor(this)
    to.addActor(this)
    this.currentSquare = Some(to)
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
  
  def getAgility = this.turnAgility

  def isStuck = this.turnAgility == 0
  
  def turnIsOver = this.turnEnded || this.isDown || (this.isStuck && this.abilityUsed)
  
  def getStatus = statuses
  
  def maxHealth = this.maxHP
  
  def currentHP = this.hp

  def takeDamage(amount: Int) =
    hp = max(hp - max(0, amount - armor), 0)

  def healDamage(amount: Int) =
    hp = min(hp + amount, maxHP)

  def isDown = hp <= 0
  
  def getAbilities = abilities