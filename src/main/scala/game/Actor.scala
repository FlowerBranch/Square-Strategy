package game
import scalafx.scene.image.Image

import java.io.FileInputStream
import scala.math.*

sealed trait Actor(val battle: Battle):

  val canBeMovedThrough: Boolean
  var armorChange: Int
  var turnAgility: Int
  var onTheMove: Option[Vector[Square]] = None
  var currentSquare: Option[Square] = None
  var isBeingPushed = false
  
  def getAgility: Int

  def getStatuses: Vector[Statuseffect]
    
  def applyStatus(statuseffect: Option[Statuseffect]): Unit

  def removeStatus(statuseffect: Statuseffect): Unit

  def takeDamage(amount: Int): Unit

  def location: Option[Square] = this.currentSquare
  
  def pushedTo(next: Square): Unit =
    if this.currentSquare.isDefined then
      this.currentSquare.head.removeActor(this)
    next.addActor(this)
    this.currentSquare = Some(next)
    
  def moveTo(next: Square): Unit =
    pushedTo(next)
    turnAgility = max(0, turnAgility - 1)
    
  def followPath() =
    if onTheMove.isDefined then
      if onTheMove.head.nonEmpty then
        val nextSquare = onTheMove.head.head
        if isBeingPushed then
          pushedTo(nextSquare)
        else
          moveTo(nextSquare)
        onTheMove = Some(onTheMove.head.tail)
      else
        onTheMove = None
    else
      if isBeingPushed then
        isBeingPushed = false
    
end Actor

case class Obstacle(in: Battle) extends Actor(in):

  val canBeMovedThrough = false
  var armorChange = 0
  var turnAgility = 0

  def getAgility = 0

  def getStatuses: Vector[Statuseffect] = Vector()

  def applyStatus(statuseffect: Option[Statuseffect]) = ()

  def removeStatus(statuseffect: Statuseffect): Unit = ()

  def takeDamage(amount: Int) = ()

case class Character(in: Battle,
                     val name: String,
                     portraitString: String,
                     startHP: Int,
                     private val defaultArmor: Int,
                     private val agility: Int,
                     private val abilities: Vector[Ability]
                    ) extends Actor(in):

  val portrait = Image(FileInputStream(portraitString))
  val canBeMovedThrough: Boolean = false
  var turnEnded = false
  var abilityUsed = false
  var armorChange = 0
  var turnAgility = this.agility
  private val maxHP = startHP
  private var hp = startHP
  private var statuses = Vector[Statuseffect]()

  def statusTick() = this.statuses.foreach(_.applyEffect(this))

  def turnStartState() =
    this.turnAgility = this.agility
    armorChange = 0
    abilityUsed = false
    turnEnded = false

  def applyStatus(statuseffect: Option[Statuseffect]) =
    if statuseffect.isDefined then
      this.statuses = this.statuses ++ Vector(statuseffect.head)

  def removeStatus(statuseffect: Statuseffect): Unit =
    this.statuses = this.statuses.filter(_.name != statuseffect.name)

  def getStatuses = this.statuses
  
  def getAgility = this.turnAgility

  def isStuck = this.turnAgility == 0
  
  def turnIsOver = this.turnEnded || this.isDown || (this.isStuck && this.abilityUsed)
  
  def maxHealth = this.maxHP
  
  def currentHP = this.hp

  def takeDamage(amount: Int) =
    hp = max(hp - max(0, amount - currentArmor), 0)

  def healDamage(amount: Int) =
    hp = min(hp + amount, maxHP)

  def currentArmor = defaultArmor + armorChange

  def isDown = hp <= 0
  
  def getAbilities = abilities