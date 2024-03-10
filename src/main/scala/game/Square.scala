package game

import game.Direction.*

case class Square(val x: Int, val y: Int):

  private var highlightStatus = false
  private var lockState = false
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