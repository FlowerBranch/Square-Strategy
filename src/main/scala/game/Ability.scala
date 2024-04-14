package game

import Direction.*

sealed trait Ability://TODO ability that pushes obstacles

  val name: String
  val status: Option[StatusEffect]
  val directions = Vector(Right, Down, Left, Up)

  def areaOfEffect(square: Square, direction: Direction): Vector[Square]

  def use(user: Actor, direction: Direction): Unit

  def calculateDamage(square: Square, user: Character): (Square, Direction, Int)

  def useWithParams(user: Actor, direction: Direction, userDamage: Int, otherDamage: Int, statusToApply: Option[StatusEffect]) =
    areaOfEffect(user.location.head, direction).filter(!_.isEmpty).map(_.getActor.head).foreach(handleActor(_))

    def handleActor(actor: Actor) =
      if actor == user then
        actor.takeDamage(userDamage)
      else
        actor.takeDamage(otherDamage)
      if statusToApply.isDefined then
        val existingStatus = actor.getStatuses.find(_.name == statusToApply.head.name)
        if existingStatus.isDefined then
          existingStatus.head.increaseDuration(statusToApply.head.duration)
        else
          actor.applyStatus(statusToApply)
      else
        ()
    end handleActor

  end useWithParams


  def calculateDamageHelper(square: Square, user: Character, userDamage: Int, otherDamage: Int): (Square, Direction, Int) =

    def calculateScore(target: Square): Int =
      
      def statusScore: Int =
        if this.status.isDefined then
          if this.status.head.isNegative then
            50
          else
            -50
        else
          0
      end statusScore
      
      var score = 0

      if target.hasCharacter then
        val character = (user.battle.playerTeam ++ user.battle.enemyTeam).find(_.location == Some(target)).head
        if character != user then
          if user.battle.enemyTeam.contains(character) then
            score += -statusScore - otherDamage
          else
            score += statusScore + otherDamage
        else
          ()
      else
        if square == target then
          score += -statusScore - userDamage
        
      score
      
    end calculateScore
 
    val possibleDamage: Vector[(Direction, Int)] =
      (for i <- 0 until 4 yield
        val score = this.areaOfEffect(square, directions(i)).map(calculateScore(_)).sum
        (directions(i), score)
        ).toVector

    val max = possibleDamage.maxBy(p => p._2)
    (square, max._1, max._2)

  end calculateDamageHelper

object Pyromania extends Ability:

  val name = "Pyromania"
  val status = Some(Burn(3))
  
  def areaOfEffect(square: Square, direction: Direction) =
    square.allNeighbors.flatMap(_.allNeighbors).distinct

  def use(user: Actor, direction: Direction): Unit =
    useWithParams(user, direction, 20, 100, Some(Burn(3)))

  def calculateDamage(square: Square, user: Character): (Square, Direction, Int) =
    calculateDamageHelper(square, user, 20, 100)

object Stab extends Ability:

  val name = "Stab"
  val status = Some(Bleeding(2))

  def areaOfEffect(square: Square, direction: Direction) =
    square.squaresInDirection(1, direction)

  def use(user: Actor, direction: Direction): Unit =
    useWithParams(user, direction, 0, 150, Some(Bleeding(2)))

  def calculateDamage(square: Square, user: Character): (Square, Direction, Int) =
    super.calculateDamageHelper(square, user, 0, 100)
    
object Earthquake extends Ability:
  
  val name = "Earthquake"
  val status = None
  
  def areaOfEffect(square: Square, direction: Direction) =
    square.allNeighbors.filter(_ != square)
    
  def use(user: Actor, direction: Direction): Unit =
    useWithParams(user, direction, 0, 100, None)
    
  def calculateDamage(square: Square, user: Character): (Square, Direction, Int) =
    super.calculateDamageHelper(square, user, 0, 100)