package game

import game.Direction.*

class Square(val x: Int, val y: Int):

  private var highlightStatus = false

  def isHighlighted = highlightStatus

  def highlightSwitch() =
    if isHighlighted then
      highlightStatus = false
    else
      highlightStatus = true

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