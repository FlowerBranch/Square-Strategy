package game

import io.*
import scalafx.animation.AnimationTimer

import java.io.{FileReader, StringReader}
import scala.util.Random
import scala.collection.mutable.Buffer

class Battle:

  val battleground = Battleground(20, 15)
  var obstacles = Vector[Obstacle]()

  addObstacles(60)
  
  val enemyAI = AI()

  val playerTeam = CharacterIO.readTeam("./data/playerteam.char", this)
  val enemyTeam = CharacterIO.readTeam("./data/enemyteam.char", this)
/*
  val playerTeam = Vector[Character](
    Character(this, "raimo1", 200, 20, 10, Vector(Pyromania, Stab, Earthquake)),
    Character(this, "raimo2", 200, 20, 15, Vector(Pyromania, Stab, Earthquake)),
    Character(this, "raimo3", 200, 20, 20, Vector(Pyromania, Stab, Earthquake)),
    Character(this, "raimo4", 200, 20, 5, Vector(Pyromania, Stab, Earthquake))
  )

  val enemyTeam = Vector[Character](
    Character(this, "jarmo1", 200, 20, 10, Vector(Pyromania, Stab, Earthquake)),
    Character(this, "jarmo2", 200, 20, 15, Vector(Pyromania, Stab, Earthquake)),
    Character(this, "jarmo3", 200, 20, 20, Vector(Pyromania, Stab, Earthquake)),
    Character(this, "jarmo4", 200, 20, 5, Vector(Pyromania, Stab, Earthquake))
  )
*/
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
    (characters zip squares).foreach(p => p._1.pushedTo(p._2))

  def addObstacles(amount: Int) =
    val squares = randomEmptySquares(amount)
    this.obstacles = squares.map(i => Obstacle(this))
    (this.obstacles zip squares).foreach(p => p._1.pushedTo(p._2))

  def playerLost = playerTeam.forall(_.isDown)

  def enemyLost = enemyTeam.forall(_.isDown)

  var playerTurnStart = true
  var enemyTurnStart = true

  def play(draw: => Unit) =

    def update() =
      if playerLost || enemyLost then
        ()
        
      if playerTeam.exists(!_.turnIsOver) then
        if playerTurnStart then
          playerTeam.foreach(_.statusTick())
          enemyTeam.foreach(_.statusTick())
          playerTurnStart = false
      else if enemyTeam.exists(!_.turnIsOver) then
        if enemyTurnStart then
          playerTeam.foreach(_.statusTick())
          enemyTeam.foreach(_.statusTick())
          enemyTurnStart = false
        enemyAI.nextAction(this)
      else
        playerTurnStart = true
        enemyTurnStart = true
        playerTeam.foreach(_.turnStartState())
        enemyTeam.foreach(_.turnStartState())
        
      draw
    end update

    val timer = AnimationTimer(
      (timestamp: Long) =>
        update()
        playerTeam.foreach(_.followPath())
        enemyTeam.foreach(_.followPath())
        obstacles.foreach(_.followPath())
    )

    timer.start()

  end play