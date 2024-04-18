package game

import Direction.*

/**
 * Describes an ability that may do different things such as dealing damage to the target or applying a status
 * Each ability also has their own area of effect that might depend on the direction of useage
 */
sealed trait Ability:

  val name: String
  val pushDistance: Int
  
  val directions = Vector(Right, Down, Left, Up)

  def status: Option[Statuseffect]
  
  def areaOfEffect(square: Square, direction: Direction): Vector[Square]

  def use(user: Actor, direction: Direction): Unit

  def calculateDamage(square: Square, user: Character): (Square, Direction, Int)

  /**
   * Moves target a specified distance without reducing its agility
   * @param actor target
   * @param user user of ability
   * @param distance max push distance
   * @param useDirection direction that the ability is used in
   */
  def push(actor: Actor, user: Actor, distance: Int, useDirection: Direction): Unit =

    def startPushing(in: Direction) =
      val squares = actor.location.head.squaresInDirection(distance, in).takeWhile(_.isEmpty)
      actor.onTheMove = Some(squares)
      actor.isBeingPushed = true

    val xDirection = user.location.head.xDirectionOf(actor.location.head)
    val yDirection = user.location.head.yDirectionOf(actor.location.head)
    if (xDirection.isDefined || yDirection.isDefined) && !(xDirection.isDefined && yDirection.isDefined) then
      if xDirection.isDefined then
        startPushing(xDirection.head)
      else
        startPushing(yDirection.head)
    else
      ()

  /**
   * Method to simplify the implementation of new abilities
   * @param user user of ability
   * @param direction use direction
   * @param userDamage damage that the user takes if affected
   * @param otherDamage damage that every other character (including teammates) takes
   */
  def useWithParams(user: Actor, direction: Direction, userDamage: Int, otherDamage: Int) =
    areaOfEffect(user.location.head, direction).filter(!_.isEmpty).map(_.getActor.head).foreach(handleActor(_))

    def handleActor(actor: Actor) =
    //handles all necessary functions for target of attack like damage, applied statuses and pushing
      if actor == user then
        actor.takeDamage(userDamage)
      else
        actor.takeDamage(otherDamage)

      if this.status.isDefined then
        val existingStatus = actor.getStatuses.find(_.name == this.status.head.name)
        if existingStatus.isDefined then
          existingStatus.head.increaseDuration(this.status.head.duration)
        else
          actor.applyStatus(this.status)
      else
        ()

      if this.pushDistance != 0 then
        push(actor, user, this.pushDistance, direction)

    end handleActor

  end useWithParams

  /**
   * Used by the AI to calculate best option for using ability.
   * Calculates a score for given square which the AI then compares to other scores
   * Returns a tuple with the square, the direction of usage which had the highest score and the score itself
   * @param square current square under examination
   * @param user user of ability
   * @param userDamage damage that the user takes if it would be affected
   * @param otherDamage damage that every other character (including teammates) would take
   */
  def calculateDamageWithParams(square: Square, user: Character, userDamage: Int, otherDamage: Int): (Square, Direction, Int) =

    def calculateScore(target: Square): Int =

      def statusScore: Int =
      //statuses have an impact of 50 on the square
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

      if !target.isEmpty && this.pushDistance != 0 then
      //pushing a character or an object has an effect on 10
        score += 10

      score

    end calculateScore

    val possibleDamage: Vector[(Direction, Int)] =
      (for i <- 0 until 4 yield
        val score = this.areaOfEffect(square, directions(i)).map(calculateScore(_)).sum
        (directions(i), score)
        ).toVector

    val max = possibleDamage.maxBy(p => p._2)
    (square, max._1, max._2)

  end calculateDamageWithParams

end Ability

/**
 * Ability that deals a little damage to the user and more to all others in a two square radius
 * Applies burn status to all affected (including user)
 */
object Pyromania extends Ability:

  val name = "Pyromania"
  val pushDistance = 0
  def status = Some(Burn(2))

  def areaOfEffect(square: Square, direction: Direction) =
    square.allNeighborsAndSelf.flatMap(_.allNeighborsAndSelf).distinct

  def use(user: Actor, direction: Direction): Unit =
    useWithParams(user, direction, 30, 100)

  def calculateDamage(square: Square, user: Character): (Square, Direction, Int) =
    calculateDamageWithParams(square, user, 30, 100)

/**
 * Ability that affects only the square directly in front of user.
 * Deals a lot of damage and applies bleeding status to target
 */
object Stab extends Ability:

  val name = "Stab"
  val pushDistance = 0
  def status = Some(Bleeding(2))

  def areaOfEffect(square: Square, direction: Direction) =
    square.squaresInDirection(1, direction)

  def use(user: Actor, direction: Direction): Unit =
    useWithParams(user, direction, 0, 120)

  def calculateDamage(square: Square, user: Character): (Square, Direction, Int) =
    super.calculateDamageWithParams(square, user, 0, 120)

/**
 * Ability that affects all non diagonal squares around user.
 * Deals a lot of damage and pushes each object and character affected two squares back if possible
 */
object Burst extends Ability:

  val name = "Burst"
  val pushDistance = 2
  def status = None

  def areaOfEffect(square: Square, direction: Direction) =
    square.nonDiagonalNeighbors

  def use(user: Actor, direction: Direction): Unit =
    useWithParams(user, direction, 0, 150)

  def calculateDamage(square: Square, user: Character): (Square, Direction, Int) =
    super.calculateDamageWithParams(square, user, 0, 150)