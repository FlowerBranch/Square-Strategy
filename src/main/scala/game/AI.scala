package game

import Direction.*

class AI:
  
  val directions = Vector(Up, Right, Down, Left)
  var enemyTurnStart = true
  var characterIndex = 0
  
  def nextAction(in: Battle) =
    if enemyTurnStart then
      in.enemyTeam.foreach(_.turnStartState())
      enemyTurnStart = false
      characterIndex = 0
    if in.enemyTeam.forall(_.onTheMove.isEmpty) then
      ()//calculateMovement(in.enemyTeam(characterIndex))
    else
      ()
 /*
    def calculateMovement(of: Character) =
  
      val possibleSquares = in.battleground.squaresWithinRadius(of.location.head, of.getAgility)
      
      val recommendation: Vector[(Square, Int)] =
        (for i <- 0 until 3 yield 
        calculateForAbility(of.getAbilities(i))).toVector
      
      def calculateForAbility(ability: Ability): (Square, Int) = ???
*/
      