package game

import Direction.*

sealed trait Ability:

  val name: String

  def areaOfEffect(user: Character, direction: Direction): Vector[Square]

  def use(user: Character, direction: Direction): Unit
  
  def dealDamage(area: Vector[Square], damage: Int) =
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

  def areaOfEffect(user: Character, direction: Direction) =
    user.location.head.allNeighbors.flatMap(_.allNeighbors).distinct

  def use(user: Character, direction: Direction): Unit =
    val area = areaOfEffect(user, direction)
    area.foreach(i =>
      if i.getActor.isDefined then
        i.getActor.head match
          case c: Character =>
            if c == user then
              c.takeDamage(20)
            else
              c.takeDamage(70)
          case _ =>
            ()
      else
        ()
    )
    
object Stab extends Ability:
  
  val name = "Stab"
  
  def areaOfEffect(user: Character, direction: Direction) =
    user.location.head.squaresInDirection(1, direction)
    
  def use(user: Character, direction: Direction): Unit =
    dealDamage(areaOfEffect(user, direction), 100)