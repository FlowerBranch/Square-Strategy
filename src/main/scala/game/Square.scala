package game

class Square(val x: Int, val y: Int):

  private var highlightStatus = false
  
  def isHighlighted = highlightStatus
  
  def highlightSwitch() =
    if isHighlighted then
      highlightStatus = false
    else
      highlightStatus = true