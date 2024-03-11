package game

import scala.util.Random

class Battle:
  
  val battleground = Battleground(20, 15)

  val obstacleLocations: Seq[(Int, Int)] =
    for i <- 0 until 10 yield
      (Random.nextInt(battleground.width), Random.nextInt(battleground.height))

  battleground.area.foreach(_.foreach(i =>
    if obstacleLocations.exists(j => i.x == j._1 && i.y == j._2) then
      i.addActor(Obstacle())
    else
      ()
  ))