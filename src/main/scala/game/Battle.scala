package game

import scalafx.animation.AnimationTimer

import scala.util.Random

class Battle:

  val battleground = Battleground(20, 15) //TODO battleground returns current square of residence
  
  val playerTeam = Vector[Character](
    Character(this, "raimo1", 200, 20, 10, Vector(Pyromania, Stab)),
    Character(this, "raimo2", 200, 20, 15, Vector(Pyromania, Stab)),
    Character(this, "raimo3", 200, 20, 20, Vector(Pyromania, Stab)),
    Character(this, "raimo4", 200, 20, 5, Vector(Pyromania, Stab))
  )
  
  val playerLocations: Seq[(Int, Int)] =
    for i <- 0 until 4 yield
      (Random.nextInt(this.battleground.width), Random.nextInt(this.battleground.height))
      
  playerTeam(0).move(this.battleground.getSquare(playerLocations(0)._1, playerLocations(0)._2).head, Vector())
  playerTeam(1).move(this.battleground.getSquare(playerLocations(1)._1, playerLocations(1)._2).head, Vector())
  playerTeam(2).move(this.battleground.getSquare(playerLocations(2)._1, playerLocations(2)._2).head, Vector())
  playerTeam(3).move(this.battleground.getSquare(playerLocations(3)._1, playerLocations(3)._2).head, Vector())
  
  val enemyTeam = Vector[Character](
    Character(this, "jarmo1", 200, 20, 10, Vector(Pyromania)),
    Character(this, "jarmo2", 200, 20, 15, Vector(Pyromania)),
    Character(this, "jarmo3", 200, 20, 20, Vector(Pyromania)),
    Character(this, "jarmo4", 200, 20, 5, Vector(Pyromania))
  )
  val enemyLocations: Seq[(Int, Int)] =
    for i <- 0 until 4 yield
      (Random.nextInt(this.battleground.width), Random.nextInt(this.battleground.height))
      
  enemyTeam(0).move(this.battleground.getSquare(enemyLocations(0)._1, enemyLocations(0)._2).head, Vector())
  enemyTeam(1).move(this.battleground.getSquare(enemyLocations(1)._1, enemyLocations(1)._2).head, Vector())
  enemyTeam(2).move(this.battleground.getSquare(enemyLocations(2)._1, enemyLocations(2)._2).head, Vector())
  enemyTeam(3).move(this.battleground.getSquare(enemyLocations(3)._1, enemyLocations(3)._2).head, Vector())
  
  battleground.addObstacles(60)

  def playerLost = playerTeam.forall(_.isDown)

  def enemyLost = enemyTeam.forall(_.isDown)

  var turnStart = true

  def play(draw: => Unit) =

    def update() =
      if playerLost || enemyLost then
        ()
      if playerTeam.exists(!_.turnIsOver) then
        if turnStart then
          playerTeam.foreach(_.turnStartState())
          turnStart = false
        ()
      else
        ()
      draw
    end update

    val timer = AnimationTimer(
      (timestamp: Long) =>
        update()
    )

    timer.start()

  end play