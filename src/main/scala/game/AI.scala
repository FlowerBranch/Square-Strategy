package game

import Direction.*

/**
 * An instance of this class will control the enemy characters during its turn
 */
class AI:

  private val directions = Vector(Right, Down, Left, Up)
  private var characterIndex = 0
  private var actionTuple: Option[(Square, Ability, Direction, Square)] = None
  private var actionIndex = 0
  private var outOfReach = false

  private def getAway(character: Character): Square =
  //This method will be used when a character either has nothing to use an ability or has already used its ability.
  //It tries to get as far away as possible from the characters in the players team.
    val radius = character.battle.battleground.squaresWithinRadius(character.location.head, character.getAgility)
    val square = radius.maxBy(i => character.battle.playerTeam.filter(!_.isDown).map(j => j.location.head.distanceTo(i._1)).sum)._1
    square

  private def moveCharacter(character: Character, to: Square): Unit =
  //Equivalent for the player clicking a character and moving it to a square but for the use of the AI
    val radius = character.battle.battleground.squaresWithinRadius(character.location.head, character.getAgility)
    val path = character.battle.battleground.squaresAlongPath(to, radius)
    character.battle.enemyTeam(characterIndex).onTheMove = path

  /**
   * The main method for the AI to perform its turn which is taken a single action at a time to improve visual effect.
   * First all the actions for one characters full turn will be calculated and then the turn is taken. 
   * @param in the battle in which this AI controls the enemy team
   */
  def nextAction(in: Battle) =

    if in.enemyTeam.forall(_.onTheMove.isEmpty) then

      val currentCharacter = in.enemyTeam(characterIndex)

      def reset() =
      //this prepares the AI for the next character and the next turn
        actionIndex = 0
        if characterIndex >= 3 then
          characterIndex = 0
          currentCharacter.turnEnded = true
        else
          characterIndex += 1
          
      if !currentCharacter.isDown then
        
        if actionTuple.isDefined then
  
          val actions = actionTuple.head
          actionIndex match //here first the character moves to the square it'll attack from, then attacks, then moves away
            case 0 =>
              moveCharacter(currentCharacter, actions._1)
              actionIndex += 1
            case 1 =>
              actions._2.use(currentCharacter, actions._3)
              actionIndex += 1
            case 2 =>
              moveCharacter(currentCharacter, actions._4)
              actionTuple = None
              currentCharacter.turnEnded = true
              reset()
            case _ => throw Exception("Something went wrong with AI trying to execute its turn")
  
        else
  
          if outOfReach then
            moveCharacter(currentCharacter, getAway(currentCharacter))
            currentCharacter.turnEnded = true
            outOfReach = false
            reset()
          else
            actionTuple = defineActions(currentCharacter)

      else
        reset()
      
    else
      ()

    def defineActions(of: Character): Option[(Square, Ability, Direction, Square)] =
    //this method calculates the whole turn for the AI
      val radius = in.battleground.squaresWithinRadius(of.location.head, of.getAgility)
      val possibleSquares = radius.map(_._1)

      def calculateForAbility(ability: Ability): Option[(Square, Ability, Direction, Int)] =
      //this calculates the optimal use of one ability
        val relevantSquares = (in.playerTeam.filter(!_.isDown) ++ in.obstacles).map(_.location.head)
        val abilitySquares = relevantSquares.flatMap(i => directions.flatMap(j => ability.areaOfEffect(i, j)).distinct).distinct
        val squaresToTest = possibleSquares.filter(i => abilitySquares.contains(i) && !relevantSquares.contains(i))

        if squaresToTest.isEmpty then
          None
        else
          val chosenOne = squaresToTest.map(i => ability.calculateDamage(i, of)).maxBy(_._3)
          Some((chosenOne._1, ability, chosenOne._2, chosenOne._3))
      end calculateForAbility
      
      val recommendations: Vector[(Square, Ability, Direction, Int)] =
        (for i <- 0 until 3 yield
        calculateForAbility(of.getAbilities(i))).flatten.toVector

      if recommendations.isEmpty then
        outOfReach = true
        None
      else
        val chosenAbility = recommendations.maxBy(_._4)
        val agilityMinus = radius.find(i => chosenAbility._1 == i._1).head._2
        val secondRadius = of.battle.battleground.squaresWithinRadius(chosenAbility._1, of.getAgility - agilityMinus)
        val secondSquare = secondRadius.maxBy(i => of.battle.playerTeam.filter(!_.isDown).map(j => j.location.head.distanceTo(i._1)).sum)._1

        Some((chosenAbility._1, chosenAbility._2, chosenAbility._3, secondSquare))