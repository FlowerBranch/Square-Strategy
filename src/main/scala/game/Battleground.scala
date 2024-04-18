package game

import scala.collection.mutable.Buffer

/**
 * Class that imitates a battleground using squares aligned in a grid format
 * @param width width in squares
 * @param height height in squares
 */
class Battleground(val width: Int, val height: Int):

  val area: Seq[Seq[Square]] =
    for i <- 0 until width yield
      for j <- 0 until height yield
        Square(i, j, this)

  var lockedSquare: Option[Square] = None
  var movementRadius: Option[Vector[(Square, Int)]] = None

  /**
   * Returns the square in that exists in the specified coordinates.
   * The square is wrapped in an option in case the given coordinates point to outside tha battleground
   * @param x x-coordinate of wanted square
   * @param y y-coordinate of wanted square
   */
  def getSquare(x: Int, y: Int): Option[Square] =
    this.area.flatMap(_.find(i =>
      i.x == x && i.y == y
    )).headOption

  /**
   * Returns all the squares in the currently existing radius of movement that the character in the locked square has
   */
  def getCurrentRadius: Vector[Square] =
    if movementRadius.isDefined then
      movementRadius.head.map(_._1)
    else
      Vector()

  /**
   * Returns all squares that can be reached in a specified number of steps from a given starting square.
   * The returned vector includes information about the number of steps required to reach each square
   * @param of starting square
   * @param depth how many steps can be taken at a maximum to reach any square
   */
  def squaresWithinRadius(of: Square, depth: Int): Vector[(Square, Int)] =

    val usedSquares = Buffer[Square]()
    //buffer is used to avoid duplicates in the result

    def squaresForNextDepth(squares: Vector[Square]): Vector[Square] =
      squares.flatMap(i => i.emptyNonDiagonalNeighbors.filterNot(i => usedSquares.contains(i))).distinct

    def squaresUntilDepth(omstart: (Vector[Square], Int)): Vector[(Square, Int)] =
    //the squares are found using a recursive algorithm
      val thisDepth = omstart._1 zip Vector.tabulate(omstart._1.size)(i => omstart._2)
      usedSquares ++= omstart._1.toBuffer
      if omstart._2 == depth then
        thisDepth
      else
        val nextDepth = squaresForNextDepth(omstart._1)
        thisDepth ++ squaresUntilDepth((nextDepth, omstart._2 + 1))

    squaresUntilDepth((Vector(of), 0))
    
  end squaresWithinRadius

  /**
   * Returns a path that can be taken in the battleground to reach a target square from the origin of the given radius.
   * The returned vector is wrapped in an option in case the end square doesn't exist in the radius
   * @param end the final square in the path
   * @param radius radius of an origin square calculated in the manner of the squaresWithinRadius method
   */
  def squaresAlongPath(end: Square, radius: Vector[(Square, Int)]): Option[Vector[Square]] =

    def moveToOrigin(from: (Square, Int)): Vector[Square] =
    //uses recursion to find the origin of the radius
      if from._2 == 0 then
        Vector()
      else if from._2 == 1 then
        Vector(from._1)
      else
        val lowerDepthRadius = radius.filter(_._2 == from._2 - 1)
        Vector(from._1) ++ moveToOrigin(lowerDepthRadius.find(p => from._1.emptyNonDiagonalNeighbors.contains(p._1)).head)

    val origin = radius.find(_._1 == end)
    if origin.isDefined then
      Some(moveToOrigin(origin.head).reverse)
    else
      None
      
  end squaresAlongPath