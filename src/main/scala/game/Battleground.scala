package game

import scala.collection.mutable.Buffer
import game.Direction.*

class Battleground(val width: Int, val height: Int):

  val area: Seq[Seq[Square]] =
    for i <- 0 until width yield
      for j <- 0 until height yield
        Square(i, j, this)

  var lockedSquare: Option[Square] = None
  var movementRadius: Option[Vector[(Square, Int)]] = None

  def getCurrentRadius: Vector[Square] =
    if movementRadius.isDefined then
      movementRadius.head.map(_._1)
    else
      Vector()

  def getSquare(x: Int, y: Int): Option[Square] =
    this.area.flatMap(_.find(i =>
      i.x == x && i.y == y
    )).headOption

  def squaresWithinRadius(of: Square, depth: Int): Vector[(Square, Int)] =

    val usedSquares = Buffer[Square]()

    def squaresForNextDepth(squares: Vector[Square]): Vector[Square] =
      squares.flatMap(i => i.emptyNeighbors.filterNot(i => usedSquares.contains(i))).distinct

    def squaresUntilDepth(omstart: (Vector[Square], Int)): Vector[(Square, Int)] =
      val thisDepth = omstart._1 zip Vector.tabulate(omstart._1.size)(i => omstart._2)
      usedSquares ++= omstart._1.toBuffer
      if omstart._2 == depth then
        thisDepth
      else
        val nextDepth = squaresForNextDepth(omstart._1)
        thisDepth ++ squaresUntilDepth((nextDepth, omstart._2 + 1))

    squaresUntilDepth((Vector(of), 0))
    
  end squaresWithinRadius

  def squaresAlongPath(end: Square, radius: Vector[(Square, Int)]): Option[Vector[Square]] =

    def moveToOrigin(from: (Square, Int)): Vector[Square] =
      if from._2 == 0 then
        Vector()
      else if from._2 == 1 then
        Vector(from._1)
      else
        val lowerDepthRadius = radius.filter(_._2 == from._2 - 1)
        Vector(from._1) ++ moveToOrigin(lowerDepthRadius.find(p => from._1.emptyNeighbors.contains(p._1)).head)

    val origin = radius.find(_._1 == end)
    if origin.isDefined then
      Some(moveToOrigin(origin.head).reverse)
    else
      None
      
  end squaresAlongPath