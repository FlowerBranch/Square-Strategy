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
  var movementRadius: Option[Vector[(Square, Int)]] = None
  var squaresWithObstacles = Buffer[Square]()

  def getCurrentRadius: Vector[Square] =
    if movementRadius.isDefined then
      movementRadius.head.map(_._1)
    else
      Vector()
  
  def addObstacles(amount: Int) =
    val obstacleLocations: Seq[(Int, Int)] =
      for i <- 0 until amount yield
        (Random.nextInt(this.width), Random.nextInt(this.height))

    area.foreach(_.foreach(i =>
      if obstacleLocations.exists(j => i.x == j._1 && i.y == j._2) then
        if i.isEmpty then
          i.addActor(Obstacle())
          squaresWithObstacles += i
      else
        ()
    ))

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

  def squaresAlongPath(end: Square, radius: Vector[(Square, Int)]): Vector[Square] =

    def moveToOrigin(from: (Square, Int)): Vector[Square] =
      if from._2 == 0 then
        throw Exception("You are already in that square")
      else if from._2 == 1 then
        Vector(from._1)
      else
        val lowerDepthRadius = radius.filter(_._2 == from._2 - 1)
        Vector(from._1) ++ moveToOrigin(lowerDepthRadius.find(p => from._1.emptyNeighbors.contains(p._1)).head)
    
    if !radius.exists(_._1 == end) then
      throw Exception("Chosen square outside radius of movement")
    else
      moveToOrigin(radius.find(_._1 == end).head).reverse
      
  end squaresAlongPath
  // ++ Vector(radius.find(p => p._2 == 0).head._1)
/*
  def followTunnel(start: Square): Vector[Square] =
    val tunnel = Buffer[Square]()
    var currentSquare = start
    while currentSquare.emptyNeighbors.filterNot(tunnel.contains(_)).size <= 1 do
      tunnel += currentSquare
      currentSquare = currentSquare.emptyNeighbors.filterNot(tunnel.contains(_)).head
    tunnel.toVector

  val tunnelSquares = area.flatMap(_.filter(_.emptyNeighbors.size <= 1)).map(followTunnel(_))

  def squaresAlongPath(start: Square, end: Square): Vector[Square] =

    val dangerSquares = tunnelSquares.filterNot(i => i.contains(end) || i.contains(start)).flatten

    var usedSquares = Buffer[Square](start)
    var path = Buffer[Square]()
    var currentSquare = start

    def addToPath(): Unit =
      if currentSquare != start && currentSquare != end then
        path += currentSquare
        usedSquares += currentSquare
        if usedSquares.length > 10 then
          usedSquares = usedSquares.tail
      else
        ()

    def move() =
      currentSquare = currentSquare.nonDiagonalNeighbors
        .filter(i => i.isEmpty
          && !usedSquares.contains(i)
          && i.emptyNeighborsExcluding(usedSquares.toVector ++ Vector(i)).nonEmpty
          && !dangerSquares.contains(i))
        .map(i => (i, i.distanceTo(end)))
        .minBy(p => p._2)._1
      addToPath()

    while !end.nonDiagonalNeighbors.exists(i => path.contains(i)) && currentSquare != end do
      move()

    Vector(start) ++ path.distinct.toVector ++ Vector(end)
  end squaresAlongPath
*/
/*    WHY IS MY LIFE LIKE THIS, ALL THE HARD WORK FOR NOTHING
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
*/