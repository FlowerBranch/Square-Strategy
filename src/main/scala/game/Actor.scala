package game

trait Actor:

  val canBeMovedThrough: Boolean

end Actor

case class Obstacle() extends Actor:

  val canBeMovedThrough = false