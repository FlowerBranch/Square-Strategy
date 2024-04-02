package game

sealed trait Ability:

  val name: String

  def areaOfEffect(user: Character, direction: Direction): Vector[Square]

  def use(user: Character, direction: Direction): Unit

object Pyromania extends Ability:

  val name = "Pyromania"

  def areaOfEffect(user: Character, direction: Direction) =
    direction match
      case _ => user.location.head.allNeighbors.flatMap(_.allNeighbors).distinct

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