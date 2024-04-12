package game

import Direction.*

class AI:
  
  val directions = Vector(Up, Right, Down, Left)
  var characterIndex = 0
  var actionTuple: Option[(Square, Ability, Direction, Square)] = None
  var actionIndex = 0


  def nextAction(in: Battle) =

    if in.enemyTeam.forall(_.onTheMove.isEmpty) then
      val currentCharacter = in.enemyTeam(characterIndex)
      if actionTuple.isDefined then

        val actions = actionTuple.head
        actionIndex match
          case 0 =>
            val radius = in.battleground.squaresWithinRadius(currentCharacter.location.head, currentCharacter.getAgility)
            val path = in.battleground.squaresAlongPath(actions._1, radius)
            in.enemyTeam(characterIndex).onTheMove = Some(path)
            actionIndex += 1
          case 1 =>
            actions._2.use(currentCharacter.location.head, actions._3)
            actionIndex += 1
          case 2 =>
            val radius = in.battleground.squaresWithinRadius(currentCharacter.location.head, currentCharacter.getAgility)
            val path = in.battleground.squaresAlongPath(actions._4, radius)
            in.enemyTeam(characterIndex).onTheMove = Some(path)
            actionTuple = None
            actionIndex = 0
            if characterIndex >= 4 then
              characterIndex = 0
              currentCharacter.turnEnded = true
            else
              characterIndex += 1
          case _ => throw Exception("Something went wrong with AI trying to execute its turn")

      else
        ()//actionTuple = Some(defineActions(currentCharacter))
    else
      ()
/*
    def defineActions(of: Character): (Square, Ability, Direction, Square) =
  
      val possibleSquares = in.battleground.squaresWithinRadius(of.location.head, of.getAgility)
      
      val recommendation: Vector[(Square, Int)] =
        (for i <- 0 until 3 yield 
        calculateForAbility(of.getAbilities(i))).toVector
      
      def calculateForAbility(ability: Ability) = ???

*/