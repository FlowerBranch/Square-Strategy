package game

import scala.collection.mutable.Buffer
import game.Direction.*

class Battleground(val width: Int, val height: Int):

  val area: Seq[Seq[Square]] =
    for i <- 0 until width yield
      for j <- 0 until height yield
        Square(i, j)
        
  var lockedSquare: Option[Square] = None

  def squaresAlongPath(start: Square, end: Square): Vector[Square] =

    var path = Buffer[Square]()
    var currentSquare = start

    def xMove() =
      if currentSquare.xDirectionOf(end).head == Right then
        currentSquare = Square(currentSquare.x + 1, currentSquare.y)
      else
        currentSquare = Square(currentSquare.x - 1, currentSquare.y)
      if currentSquare != start && currentSquare != end then
        path += currentSquare

    def yMove() =
      if currentSquare.yDirectionOf(end).head == Down then
        currentSquare = Square(currentSquare.x, currentSquare.y + 1)
      else
        currentSquare = Square(currentSquare.x, currentSquare.y - 1)
      if currentSquare != start && currentSquare != end then
        path += currentSquare

    while currentSquare.xDirectionOf(end).isDefined do
      xMove()
    while currentSquare.yDirectionOf(end).isDefined do
      yMove()

    Vector(start) ++ path.toVector ++ Vector(end)

  end squaresAlongPath