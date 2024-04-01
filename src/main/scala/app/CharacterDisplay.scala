package app

import game.Character
import scalafx.geometry.Insets
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Label
import scalafx.scene.layout.{ColumnConstraints, GridPane, RowConstraints}
import scalafx.scene.paint.Color.{Black, Blue}

def makeGrid(ofDimX: Int, ofDimY: Int) =
  val grid =  new GridPane():
    gridLinesVisible = true
  val column = new ColumnConstraints:
    percentWidth = 100.0 / ofDimX.toDouble
  val row = new RowConstraints:
    percentHeight = 100.0 / ofDimY.toDouble
  grid.columnConstraints =
    for i <- 0 until ofDimX yield
      column
  grid.rowConstraints =
    for i <- 0 until ofDimY yield
      row
  grid
end makeGrid

class CharacterDisplay(of: Character):

  val characterBox = makeGrid(2, 2)
  characterBox.margin = Insets(3, 3, 3, 3)
  val shifter = "    "
  
  val portrait = new Canvas():
    width = 50
    height = 50
    if of.battle.playerTeam.contains(of) then
      graphicsContext2D.setFill(Blue)
      graphicsContext2D.fillOval(0, 10, width.toDouble, height.toDouble - 10)
    else
      graphicsContext2D.setFill(Black)
      graphicsContext2D.fillOval(0, 10, width.toDouble, height.toDouble - 10)
  val name = Label(shifter + of.name)
  var health = Label(shifter + s"HP: ${of.currentHP}/${of.maxHealth}")
  var status = Label(shifter + "Status: " + of.getStatus.mkString)
  
  characterBox.add(portrait, 0, 0, 1, 1)
  characterBox.add(name, 1, 0, 1, 1)
  characterBox.add(health, 0, 1, 1, 1)
  characterBox.add(status, 1, 1, 1, 1)
  
  def update() =
    if characterBox.children.nonEmpty then
      characterBox.children.remove(0, characterBox.children.size - 1)
    health = Label(shifter + s"HP: ${of.currentHP}/${of.maxHealth}")
    status = Label(shifter + "Status: " + of.getStatus.mkString)
    characterBox.add(portrait, 0, 0, 1, 1)
    characterBox.add(name, 1, 0, 1, 1)
    characterBox.add(health, 0, 1, 1, 1)
    characterBox.add(status, 1, 1, 1, 1)