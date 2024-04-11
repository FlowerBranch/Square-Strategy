package game

class AI:
  
  var enemyTurnStart = true
  var characterIndex = 0
  
  def nextAction(in: Battle) =
    if enemyTurnStart then
      in.enemyTeam.foreach(_.turnStartState())
      enemyTurnStart = false
      characterIndex = 0
    if in.enemyTeam.forall(_.onTheMove.isEmpty) then
      ()
    else
      ()
    