package game

trait Status(val duration: Int):
  
  val name: String
  
  def applyEffect(target: Character): Unit