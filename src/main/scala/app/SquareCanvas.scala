package app

import game.*
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.paint.Color.{Brown, Grey, LightBlue, Yellow}

/**
 * Is responsible for drawing the content of one square on the GUI.
 * @param square square that the end result of the drawing is based on
 */
class SquareCanvas(val square: Square):

  val canvas = new Canvas():
    width = 50
    height = 50

  /**
   * Redraws the square with possibly updated parameters.
   * Is called every frame for each square
   */
  def redraw() =
    if square.isHighlighted || square.isLocked then
      canvas.graphicsContext2D.setFill(Yellow)
      canvas.graphicsContext2D.fillRect(0, 0, canvas.width.toDouble, canvas.height.toDouble)
    else if square.battleground.getCurrentRadius.contains(square) then
      canvas.graphicsContext2D.setFill(LightBlue)
      canvas.graphicsContext2D.fillRect(0, 0, canvas.width.toDouble, canvas.height.toDouble)
    else
      canvas.graphicsContext2D.setFill(Brown)
      canvas.graphicsContext2D.fillRect(0, 0, canvas.width.toDouble, canvas.height.toDouble)
    if !square.isEmpty then
      square.getActor.head match
        case obs: Obstacle =>
          canvas.graphicsContext2D.setFill(Grey)
          canvas.graphicsContext2D.fillRect(0, 10, canvas.width.toDouble, canvas.height.toDouble - 10)
        case c: Character =>
          canvas.graphicsContext2D.drawImage(c.portrait, 0, 0, 50, 50)