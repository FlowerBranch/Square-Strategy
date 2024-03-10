package app

import game.Square
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.paint.Color.Blue

class SquareCanvas(val square: Square):

  val canvas = new Canvas():
    width = 50
    height = 50
    graphicsContext2D.setFill(Blue)
    graphicsContext2D.fillRect(0, 0, width.toDouble, height.toDouble)