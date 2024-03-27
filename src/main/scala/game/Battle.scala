package game

import scala.util.Random

class Battle:

  val battleground = Battleground(20, 15)
  
  val playerTeam = Vector[Character](
    Character("raimo1", 200, 20, 10),
    Character("raimo2", 200, 20, 15),
    Character("raimo3", 200, 20, 20),
    Character("raimo4", 200, 20, 5)
  )
  
  val playerLocations: Seq[(Int, Int)] =
    for i <- 0 until 4 yield
      (Random.nextInt(this.battleground.width), Random.nextInt(this.battleground.height))
      
  playerTeam(0).move(this.battleground.getSquare(playerLocations(0)._1, playerLocations(0)._2).head)
  playerTeam(1).move(this.battleground.getSquare(playerLocations(1)._1, playerLocations(1)._2).head)
  playerTeam(2).move(this.battleground.getSquare(playerLocations(2)._1, playerLocations(2)._2).head)
  playerTeam(3).move(this.battleground.getSquare(playerLocations(3)._1, playerLocations(3)._2).head)
  
  battleground.addObstacles(60)