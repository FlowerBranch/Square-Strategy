package game

sealed trait Ability:

  val name: String

object Pyromania extends Ability:
  
  val name = "Pyromania"
  
  def areaOfEffect(user: Character) =
    user.location.head.allNeighbors.flatMap(_.allNeighbors).distinct

  def use(user: Character): Unit =
    val area = areaOfEffect(user)
    area.foreach(i =>
      if i.getActor.isDefined then
        i.getActor.head match
          case c: Character =>
            if c == user then
              c.takeDamage(10)
            else
              c.takeDamage(25)
          case _ =>
            ()
      else
        ()
    )