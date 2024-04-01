package game

import scala.util.Random

class Battle:

  val battleground = Battleground(20, 15)
  
  val playerTeam = Vector[Character](
    Character(this, "raimo1", 200, 20, 10, Vector(Pyromania)),
    Character(this, "raimo2", 200, 20, 15, Vector(Pyromania)),
    Character(this, "raimo3", 200, 20, 20, Vector(Pyromania)),
    Character(this, "raimo4", 200, 20, 5, Vector(Pyromania))
  )
  
  val playerLocations: Seq[(Int, Int)] =
    for i <- 0 until 4 yield
      (Random.nextInt(this.battleground.width), Random.nextInt(this.battleground.height))
      
  playerTeam(0).move(this.battleground.getSquare(playerLocations(0)._1, playerLocations(0)._2).head)
  playerTeam(1).move(this.battleground.getSquare(playerLocations(1)._1, playerLocations(1)._2).head)
  playerTeam(2).move(this.battleground.getSquare(playerLocations(2)._1, playerLocations(2)._2).head)
  playerTeam(3).move(this.battleground.getSquare(playerLocations(3)._1, playerLocations(3)._2).head)
  
  val enemyTeam = Vector[Character](
    Character(this, "jarmo1", 200, 20, 10, Vector(Pyromania)),
    Character(this, "jarmo2", 200, 20, 15, Vector(Pyromania)),
    Character(this, "jarmo3", 200, 20, 20, Vector(Pyromania)),
    Character(this, "jarmo4", 200, 20, 5, Vector(Pyromania))
  )
  val enemyLocations: Seq[(Int, Int)] =
    for i <- 0 until 4 yield
      (Random.nextInt(this.battleground.width), Random.nextInt(this.battleground.height))
      
  enemyTeam(0).move(this.battleground.getSquare(enemyLocations(0)._1, enemyLocations(0)._2).head)
  enemyTeam(1).move(this.battleground.getSquare(enemyLocations(1)._1, enemyLocations(1)._2).head)
  enemyTeam(2).move(this.battleground.getSquare(enemyLocations(2)._1, enemyLocations(2)._2).head)
  enemyTeam(3).move(this.battleground.getSquare(enemyLocations(3)._1, enemyLocations(3)._2).head)
  
  battleground.addObstacles(60)