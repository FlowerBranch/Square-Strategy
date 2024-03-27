package app

import game.*
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.paint.Color.{Black, Blue, Brown, Grey, Red}

class SquareCanvas(val square: Square):

  val canvas = new Canvas():
    width = 50
    height = 50
    if square.battleground.getCurrentRadius.contains(square) then
      graphicsContext2D.setFill(Grey)
      graphicsContext2D.fillRect(0, 0, width.toDouble, height.toDouble)
    else
      graphicsContext2D.setFill(Blue)
      graphicsContext2D.fillRect(0, 0, width.toDouble, height.toDouble)
    if !square.isEmpty then
      square.getActor.head match
        case obs: Obstacle =>
          graphicsContext2D.setFill(Brown)
          graphicsContext2D.fillRect(0, 10, width.toDouble, height.toDouble - 10)
        case c: Character =>
          graphicsContext2D.setFill(Black)
          graphicsContext2D.fillOval(0, 10, width.toDouble, height.toDouble - 10)

  def redraw() =
    if square.isHighlighted || square.isLocked then
      canvas.graphicsContext2D.setFill(Red)
      canvas.graphicsContext2D.fillRect(0, 0, canvas.width.toDouble, canvas.height.toDouble)
    else if square.battleground.getCurrentRadius.contains(square) then
      canvas.graphicsContext2D.setFill(Grey)
      canvas.graphicsContext2D.fillRect(0, 0, canvas.width.toDouble, canvas.height.toDouble)
    else
      canvas.graphicsContext2D.setFill(Blue)
      canvas.graphicsContext2D.fillRect(0, 0, canvas.width.toDouble, canvas.height.toDouble)
    if !square.isEmpty then
      square.getActor.head match
        case obs: Obstacle =>
          canvas.graphicsContext2D.setFill(Brown)
          canvas.graphicsContext2D.fillRect(0, 10, canvas.width.toDouble, canvas.height.toDouble - 10)
        case c: Character =>
          canvas.graphicsContext2D.setFill(Black)
          canvas.graphicsContext2D.fillOval(0, 10, canvas.width.toDouble, canvas.height.toDouble - 10)