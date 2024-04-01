package game

sealed trait Ability

object Pyromania extends Ability:

  def use(user: Character): Unit =
    val area = user.location.head.allNeighbors.flatMap(_.allNeighbors).distinct
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