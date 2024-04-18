package game

import game.Direction.*
import scala.math.*

/**
 * Class that represents a single square on the battleground.
 * Contains many methods that are essential for proper functioning of the program
 * @param x x-coordinate in battleground
 * @param y y-coordinate in battleground
 * @param battleground battleground which this square belongs to
 */
case class Square(val x: Int, val y: Int, val battleground: Battleground):

  private var highlightStatus = false
  private var lockState = false
  private var actor: Option[Actor] = None

  /**
   * Checks if this square contains an actor
   */
  def isEmpty = this.actor.isEmpty

  /**
   * Checks if this square contains an actor that is also a character
   */
  def hasCharacter =
    if this.actor.isDefined then
      this.actor.head match
        case character: Character => true
        case _                    => false
    else
      false

  /**
   * @return actor currently in this square wrapped in an option
   */
  def getActor: Option[Actor] = actor

  /**
   * Places specified actor in this square
   * @param actor actor to be placed in this square
   */
  def addActor(actor: Actor) =
    this.actor = Some(actor)

  /**
   * Removes any actor that may have been previously in this square
   */
  def removeActor() =
    this.actor = None

  /**
   * Checks whether or not this square should be highlighted in the GUI
   */
  def isHighlighted = highlightStatus

  /**
   * Switches the boolean value that determines whether this square should be higlighted in the GUI
   */
  def highlightSwitch() =
    if isHighlighted then
      highlightStatus = false
    else
      highlightStatus = true

  /**
   * Checks if this square is in a locked state mainly for the purposes of the GUI
   */
  def isLocked = lockState

  /**
   * Switches the boolean value that deteermines whether this square should be in a locked state
   */
  def lockSwitch() =
    if isLocked then
      lockState = false
    else
      lockState = true

  /**
   * Returns the square that is immediately next to this one in the given direction.
   * The square is wrapped in an option in case the check happens outside the battleground
   * @param direction direction of desired square in relation to this one
   */
  def squareInDirection(direction: Direction): Option[Square] =
    direction match
      case Right => this.battleground.getSquare(this.x + 1, this.y    )
      case Down  => this.battleground.getSquare(this.x,     this.y + 1)
      case Left  => this.battleground.getSquare(this.x - 1, this.y    )
      case Up    => this.battleground.getSquare(this.x,     this.y - 1)

  /**
   * Returns a vector with a specified amount of squares in a given direction.
   * The returned amount may be less if squares would have to be from outside the battleground
   * @param amount number of squares in the returned vector
   * @param direction direction of desired square in relation to this one
   */
  def squaresInDirection(amount: Int, direction: Direction): Vector[Square] =
    val nextSquare = this.squareInDirection(direction)
    if nextSquare.isEmpty || amount == 0 then
      Vector()
    else
      Vector(nextSquare.head) ++ nextSquare.head.squaresInDirection(amount - 1, direction)

  /**
   * Returns the direction of a given square in relation to this on the y-axis wrapped in an option.
   * If the squares are on the same level, returns None
   * @param another square that is to be examined
   */
  def yDirectionOf(another: Square): Option[Direction] =
    if this.y < another.y then
      Some(Down)
    else if this.y > another.y then
      Some(Up)
    else
      None
  /**
   * Returns the direction of a given square in relation to this on the x-axis wrapped in an option.
   * If the squares are on the same level, returns None
   * @param another square that is to be examined
   */
  def xDirectionOf(another: Square): Option[Direction] =
    if this.x < another.x then
      Some(Right)
    else if this.x > another.x then
      Some(Left)
    else
      None

  /**
   * Returns all the non diagonal neigbors of this square if they exist on the battleground
   */
  def nonDiagonalNeighbors: Vector[Square] =
    Vector(this.battleground.getSquare(this.x - 1, this.y),
           this.battleground.getSquare(this.x + 1, this.y),
           this.battleground.getSquare(this.x, this.y - 1),
           this.battleground.getSquare(this.x, this.y + 1)
    ).flatten

  /**
   * Returns this square and all the neighbors of this square including diagonals if they exist on the battleground
   */
  def allNeighborsAndSelf: Vector[Square] =
  Vector(this.battleground.getSquare(this.x - 1, this.y - 1), this.battleground.getSquare(this.x, this.y - 1), this.battleground.getSquare(this.x + 1, this.y - 1),
         this.battleground.getSquare(this.x - 1, this.y),     this.battleground.getSquare(this.x, this.y),     this.battleground.getSquare(this.x + 1, this.y),
         this.battleground.getSquare(this.x - 1, this.y + 1), this.battleground.getSquare(this.x, this.y + 1), this.battleground.getSquare(this.x + 1, this.y + 1)
  ).flatten

  /**
   * Returns all the non diagonal neigbors of this square that don't contain an actor if they exist on the battleground
   */
  def emptyNonDiagonalNeighbors =
    nonDiagonalNeighbors.filter(i => i.isEmpty)

  /**
   * Returns the distance between this square and another square given by the Pythagorean theorem
   * @param another square that will be used in the calculation with this square
   */
  def distanceTo(another: Square): Double =
    sqrt(pow(abs(another.x - this.x), 2) + pow(abs(another.y - this.y), 2))

  override def toString: String = s"(${this.x}, ${this.y})"