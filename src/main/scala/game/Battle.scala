package game

import scalafx.animation.AnimationTimer
import scala.util.Random
import scala.collection.mutable.Buffer

class Battle:

  
  val battleground = Battleground(20, 15)

  addObstacles(60)
  
  val enemyAI = AI()
  
  val playerTeam = Vector[Character](
    Character(this, "raimo1", 200, 20, 10, Vector(Pyromania, Stab)),
    Character(this, "raimo2", 200, 20, 15, Vector(Pyromania, Stab)),
    Character(this, "raimo3", 200, 20, 20, Vector(Pyromania, Stab)),
    Character(this, "raimo4", 200, 20, 5, Vector(Pyromania, Stab))
  )

  val enemyTeam = Vector[Character](
    Character(this, "jarmo1", 200, 20, 10, Vector(Pyromania, Stab)),
    Character(this, "jarmo2", 200, 20, 15, Vector(Pyromania, Stab)),
    Character(this, "jarmo3", 200, 20, 20, Vector(Pyromania, Stab)),
    Character(this, "jarmo4", 200, 20, 5, Vector(Pyromania, Stab))
  )

  placeCharacters(playerTeam, enemyTeam)

  def randomEmptySquares(amount: Int): Vector[Square] =
    val squares = Buffer[Square]()
    while squares.size < amount do
      val location = (Random.nextInt(this.battleground.width), Random.nextInt(this.battleground.height))
      val candidateSquare = this.battleground.getSquare(location._1, location._2).head
      if candidateSquare.isEmpty && !squares.contains(candidateSquare) then
        squares += candidateSquare
    squares.toVector

  def placeCharacters(player: Vector[Character], enemy: Vector[Character]): Unit =
    val characters = player ++ enemy
    val squares = randomEmptySquares(8)
    (characters zip squares).foreach(p => p._1.move(p._2, Vector()))

  def addObstacles(amount: Int) =
    val squares = randomEmptySquares(amount)
    squares.foreach(_.addActor(Obstacle(this)))

  def playerLost = playerTeam.forall(_.isDown)

  def enemyLost = enemyTeam.forall(_.isDown)

  def play(draw: => Unit) =

    def update() =
      if playerLost || enemyLost then
        ()
        
      if playerTeam.exists(!_.turnIsOver) then
        ()
      else if enemyTeam.exists(!_.turnIsOver) then
        enemyAI.nextAction(this)
      else
        playerTeam.foreach(_.turnStartState())
        enemyTeam.foreach(_.turnStartState())
        
      draw
    end update

    val timer = AnimationTimer(
      (timestamp: Long) =>
        update()
        playerTeam.foreach(_.followPath())
        enemyTeam.foreach(_.followPath())
    )

    timer.start()

  end play