package game

import game.Direction.*
import scala.math.*

case class Square(val x: Int, val y: Int, val battleground: Battleground):

  private var highlightStatus = false
  private var lockState = false
  var usedInPath = false
  private var actor: Option[Actor] = None

  def isEmpty = this.actor.isEmpty
  
  def getActor: Option[Actor] = actor
  
  def addActor(actor: Actor) =
    this.actor = Some(actor)
  
  def isHighlighted = highlightStatus

  def highlightSwitch() =
    if isHighlighted then
      highlightStatus = false
    else
      highlightStatus = true

  def isLocked = lockState
  
  def lockSwitch() =
    if isLocked then
      lockState = false
    else
      lockState = true

  def yDirectionOf(another: Square): Option[Direction] =
    if this.y < another.y then
      Some(Down)
    else if this.y > another.y then
      Some(Up)
    else
      None

  def xDirectionOf(another: Square): Option[Direction] =
    if this.x < another.x then
      Some(Right)
    else if this.x > another.x then
      Some(Left)
    else
      None

  def nonDiagonalNeighbors: Vector[Square] =
    Vector(this.battleground.getSquare(this.x - 1, this.y),
           this.battleground.getSquare(this.x + 1, this.y),
           this.battleground.getSquare(this.x, this.y - 1),
           this.battleground.getSquare(this.x, this.y + 1)
    ).flatten

  def distanceTo(another: Square) =
    sqrt(pow(abs(another.x - this.x), 2) + pow(abs(another.y - this.y), 2))