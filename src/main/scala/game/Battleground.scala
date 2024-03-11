package game

import scala.collection.mutable.Buffer
import game.Direction.*

import scala.util.Random

class Battleground(val width: Int, val height: Int):

  val area: Seq[Seq[Square]] =
    for i <- 0 until width yield
      for j <- 0 until height yield
        Square(i, j, this)
        
  var lockedSquare: Option[Square] = None

  var squaresWithObstacles = Buffer[Square]()

  def addObstacles(amount: Int) =
    val obstacleLocations: Seq[(Int, Int)] =
      for i <- 0 until amount yield
        (Random.nextInt(this.width), Random.nextInt(this.height))

    area.foreach(_.foreach(i =>
      if obstacleLocations.exists(j => i.x == j._1 && i.y == j._2) then
        i.addActor(Obstacle())
        squaresWithObstacles += i
      else
        ()
    ))

  def getSquare(x: Int, y: Int): Option[Square] =
    this.area.flatMap(_.find(i =>
      i.x == x && i.y == y
    )).headOption

  def squaresAlongPath(start: Square, end: Square): Vector[Square] =

    area.foreach(_.foreach(_.usedInPath = false))
    var path = Buffer[Square]()
    var currentSquare = start
    var firstEmptying = true
    end.usedInPath = true

    def tryXMove =
      if currentSquare.xDirectionOf(end) == Some(Right) then
        getSquare(currentSquare.x + 1, currentSquare.y)
      else
        getSquare(currentSquare.x - 1, currentSquare.y)

    def tryYMove =
      if currentSquare.yDirectionOf(end) == Some(Down) then
        getSquare(currentSquare.x, currentSquare.y + 1)
      else
        getSquare(currentSquare.x, currentSquare.y - 1)

    def addToPath(): Unit =
      if currentSquare != start && currentSquare != end then
        path += currentSquare
      else
        ()

    def moveXFirst: Boolean =
      if currentSquare.xDirectionOf(end).isDefined && tryXMove.isDefined && tryXMove.head.isEmpty then
        currentSquare = tryXMove.head
        true
      else if currentSquare.yDirectionOf(end).isDefined && tryYMove.isDefined && tryYMove.head.isEmpty then
        currentSquare = tryYMove.head
        true
      else
        false

    def moveYFirst: Boolean =
      if currentSquare.yDirectionOf(end).isDefined && tryYMove.isDefined && tryYMove.head.isEmpty then
        currentSquare = tryYMove.head
        true
      else if currentSquare.xDirectionOf(end).isDefined && tryXMove.isDefined && tryXMove.head.isEmpty then
        currentSquare = tryXMove.head
        true
      else
        false

    def move() =
      if moveXFirst then
        addToPath()
      else if moveYFirst then
        if firstEmptying then
          path = Buffer[Square]()
          firstEmptying = false
        addToPath()
      else
        path = Buffer[Square]()
        path ++= squaresAlongPath(start, end.nonDiagonalNeighbors
          .filter(i => i.isEmpty && !i.usedInPath)
          .map(i => (i, i.distanceTo(start)))
          .minBy(p => p._2)._1
        )

    while !end.nonDiagonalNeighbors.exists(i => path.contains(i)) do
      move()

    path.toVector ++ Vector(end)

  end squaresAlongPath