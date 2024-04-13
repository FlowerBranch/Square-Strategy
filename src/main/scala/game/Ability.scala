package game

import Direction.*

sealed trait Ability://TODO ability that pushes obstacles

  val name: String
  val directions = Vector(Right, Down, Left, Up)

  def areaOfEffect(square: Square, direction: Direction): Vector[Square]

  def use(square: Square, direction: Direction): Unit

  def calculateDamage(square: Square, user: Character): (Square, Direction, Int)

  def calculateDamageHelper(square: Square, user: Character, userDamage: Int, otherDamage: Int): (Square, Direction, Int) =

    val possibleDamage: Vector[(Direction, Int)] =
      (for i <- 0 until 4 yield
        (directions(i),
          (this.areaOfEffect(square, directions(i)).map(j =>
            if j.hasCharacter then
              val c = j.getActor
              if c == Some(user) then
                0
              else if c.head.battle.playerTeam.contains(c.head) then
                0 + otherDamage
              else
                -20 - otherDamage
            else
              0
          )).sum - userDamage
        )).toVector

    val max = possibleDamage.maxBy(p => p._2)
    (square, max._1, max._2)

  end calculateDamageHelper

  def dealDamage(area: Vector[Square], damage: Int): Unit =
    area.foreach(i =>
      if i.getActor.isDefined then
        i.getActor.head match
          case c: Character => c.takeDamage(damage)
          case _ => ()
      else
        ()
    )

object Pyromania extends Ability:

  val name = "Pyromania"

  def areaOfEffect(square: Square, direction: Direction) =
    square.allNeighbors.flatMap(_.allNeighbors).distinct

  def use(square: Square, direction: Direction): Unit =
    val area = areaOfEffect(square, direction)
    area.foreach(i =>
      if i.getActor.isDefined then
        i.getActor.head match
          case c: Character =>
            if c == square.getActor.head then
              c.takeDamage(20)
            else
              c.takeDamage(70)
          case _ =>
            ()
      else
        ()
    )

  override def calculateDamage(square: Square, user: Character): (Square, Direction, Int) =
    calculateDamageHelper(square, user, 21, 70)
    
object Stab extends Ability:
  
  val name = "Stab"
  
  def areaOfEffect(square: Square, direction: Direction) =
    square.squaresInDirection(1, direction)
    
  def use(square: Square, direction: Direction): Unit =
    dealDamage(areaOfEffect(square, direction), 100)

  def calculateDamage(square: Square, user: Character): (Square, Direction, Int) =
    super.calculateDamageHelper(square, user, 0, 100)