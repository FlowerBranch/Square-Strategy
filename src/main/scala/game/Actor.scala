package game

import scalafx.scene.image.Image
import java.io.FileInputStream
import scala.math.*

/**
 * Describes an Actor that can have different properties and reside in one of the battleground's squares.
 * Takes the battle it belongs to as a parameter
 */
sealed trait Actor(val battle: Battle):

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

  /**
   * Moves or "pushes" the actor to a given square and removes it from its current one.
   * Pushing is defined as the actor not losing any of its agility from its movement
   * @param next square that the actor will be moved to
   */
  def pushedTo(next: Square): Unit =
    if this.currentSquare.isDefined then
      this.currentSquare.head.removeActor()
    next.addActor(this)
    this.currentSquare = Some(next)

  /**
   * Same as pushedTo method except the actor loses one agility point for this turn
   * @param next square that the actor will be moved to
   */
  def moveTo(next: Square): Unit =
    pushedTo(next)
    turnAgility = max(0, turnAgility - 1)

  /**
   * This method allows actors to appear moving in the GUI as it's called once every frame of the timer.
   * It uses the path in the onTheMove variable to move exactly one square forward in that path.
   * It then changes the path to only contain the tail of it
   */
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

/**
 * Represents an obstacle on the battleground.
 * Doesn't have any interesting properties apart from those implemented in the actor trait
 * @param in the battle this obstacle belongs to
 */
case class Obstacle(in: Battle) extends Actor(in):

  var armorChange = 0
  var turnAgility = 0

  def getAgility = 0

  def getStatuses: Vector[Statuseffect] = Vector()

  def applyStatus(statuseffect: Option[Statuseffect]) = ()

  def removeStatus(statuseffect: Statuseffect): Unit = ()

  def takeDamage(amount: Int) = ()

/**
 * Describes a character in the battle that can use abilities and be moved by the player or an AI.
 * @param in the battle the character participates in
 * @param name name of the character
 * @param portraitString file path that points to a picture that represents the character
 * @param startHP starting health points
 * @param defaultArmor amount of armor character has without any external effects
 * @param agility how many steps the character can take during one turn
 * @param abilities vector containing references to abilities which the character can use
 */
case class Character(in: Battle,
                     val name: String,
                     portraitString: String,
                     startHP: Int,
                     private val defaultArmor: Int,
                     private val agility: Int,
                     private val abilities: Vector[Ability]
                    ) extends Actor(in):

  val portrait = Image(FileInputStream(portraitString))
  var turnEnded = false
  var abilityUsed = false
  var armorChange = 0
  var turnAgility = this.agility
  private val maxHP = startHP
  private var hp = startHP
  private var statuses = Vector[Statuseffect]()

  /**
   * Makes all of this characters statuses apply their effect once
   */
  def statusTick() = this.statuses.foreach(_.applyEffect(this))

  /**
   * Is used to reset some crucial variables at the start of the characters turn
   */
  def turnStartState() =
    this.turnAgility = this.agility
    armorChange = 0
    abilityUsed = false
    turnEnded = false

  /**
   * Adds a statuseffect to this character or prolongs an existing one
   * @param statuseffect status to be applied
   */
  def applyStatus(statuseffect: Option[Statuseffect]) =
    if statuseffect.isDefined then
      this.statuses = this.statuses ++ Vector(statuseffect.head)

  /**
   * Removes specified status from this character if the status was applied in the first place
   * @param statuseffect status to be removed
   */
  def removeStatus(statuseffect: Statuseffect): Unit =
    this.statuses = this.statuses.filter(_.name != statuseffect.name)

  def getStatuses = this.statuses
  
  def getAgility = this.turnAgility

  private def isStuck = this.turnAgility == 0

  /**
   * Combines all possibilities to determine whether this character may do something for the rest of the teams turn
   */
  def turnIsOver = this.turnEnded || this.isDown || (this.isStuck && this.abilityUsed)
  
  def maxHealth = this.maxHP
  
  def currentHP = this.hp

  /**
   * Reduces characters hp by specified amount.
   * Could be used in the future to also heal the character with a few changes to the implementation
   * @param amount of damage that is to be dealt to the character
   */
  def takeDamage(amount: Int) =
    hp = max(hp - max(0, amount - currentArmor), 0)

  def currentArmor = defaultArmor + armorChange

  def isDown = hp <= 0
  
  def getAbilities = abilities