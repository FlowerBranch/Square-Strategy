package game

import io.*
import scalafx.animation.AnimationTimer
import scala.util.Random
import scala.collection.mutable.Buffer

/**
 * This class controls the flow of the whole battle and is mainly responsible for relaying information to the GUI
 */
class Battle:

  val battleground = Battleground(20, 15)
  var obstacles = Vector[Obstacle]()
  
  addObstacles(60)
  
  private val enemyAI = AI()
  private var playerTurnStart = true
  private var enemyTurnStart = true
  private var shownOnce = false

  val playerTeam = CharacterIO.readTeam("./data/playerteam.char", this)
  val enemyTeam = CharacterIO.readTeam("./data/enemyteam.char", this)

  placeCharacters(playerTeam, enemyTeam)
  
  def playerLost = playerTeam.forall(_.isDown)

  def enemyLost = enemyTeam.forall(_.isDown)

  /**
   * This method starts the battle and keeps it going until one of the teams is fully eliminated.
   * One turn consists of moving and attacking with all the characters in ones team.
   * Uses an animation timer to draw the battle in frames
   * @param draw funtion that draws the elements based on info from the battle instance 
   * @param onceEnds function that is run once the game is over
   */
  def play(draw: => Unit, onceEnds: => Unit) =

    def update() =
      if playerLost || enemyLost then
        if !shownOnce then
          onceEnds
          shownOnce = true

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

  private def randomEmptySquares(amount: Int): Vector[Square] =
    val squares = Buffer[Square]()
    while squares.size < amount do
      val location = (Random.nextInt(this.battleground.width), Random.nextInt(this.battleground.height))
      val candidateSquare = this.battleground.getSquare(location._1, location._2).head
      if candidateSquare.isEmpty && !squares.contains(candidateSquare) then
        squares += candidateSquare
    squares.toVector

  private def placeCharacters(player: Vector[Character], enemy: Vector[Character]): Unit =
    val characters = player ++ enemy
    val squares = randomEmptySquares(8)
    (characters zip squares).foreach(p => p._1.pushedTo(p._2))

  private def addObstacles(amount: Int) =
    val squares = randomEmptySquares(amount)
    this.obstacles = squares.map(i => Obstacle(this))
    (this.obstacles zip squares).foreach(p => p._1.pushedTo(p._2))

end Battle