package app

import app.Main.stage
import game.Battleground
import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.{Background, ColumnConstraints, GridPane, VBox, RowConstraints}
import scalafx.scene.paint.Color.*

object Main extends JFXApp3:

  def start() =

    val battleground = Battleground(20, 15)

    val root = new GridPane():
      maxWidth = 1600
      maxHeight = 900

    val scene = Scene(parent = root)

    constructBackground()
    drawBattleground()

    stage = new JFXApp3.PrimaryStage:
      title = "Square Strategy"
      width = 800
      height = 450

    stage.setFullScreen(true)
    stage.scene = scene

    def constructBackground() =

      val backdrop = new Canvas():
        width = 1600
        height = 900
        graphicsContext2D.setFill(Black)
        graphicsContext2D.fillRect(0, 0, width.toDouble, height.toDouble)
      val sideBox = new VBox():
        background = Background.fill(Green)

      root.add(backdrop, 0, 0, 1, 2)
      root.add(sideBox, 1, 0, 1, 1)

      val row0 = new RowConstraints:
        percentHeight = 100
      val column0 = new ColumnConstraints:
        percentWidth = 75
      val column1 = new ColumnConstraints:
        percentWidth = 25
      root.rowConstraints = Array(row0)
      root.columnConstraints = Array(column0, column1)

    end constructBackground

    def drawBattleground() =

      val battleGrid = new GridPane():
        gridLinesVisible = true
        margin = Insets(3, 3, 3, 3)

      val battleSquares = battleground.area.map(_.map(i =>
        new Canvas():
          width = 50
          height = 50
          graphicsContext2D.setFill(Blue)
          graphicsContext2D.fillRect(0, 0, width.toDouble, height.toDouble)
      ))

      battleground.area.foreach(i => i.foreach(j =>
        battleGrid.add(battleSquares(j.x)(j.y), j.x, j.y)
      ))

      val battleColumn = new ColumnConstraints:
        percentWidth = 1.0 / battleground.width.toDouble * 100.0
      val battleRow = new RowConstraints:
        percentHeight = 1.0 / battleground.height.toDouble * 100.0
      battleGrid.columnConstraints =
        for i <- 0 until battleground.width yield
          battleColumn
      battleGrid.rowConstraints =
        for i <- 0 until battleground.height yield
          battleRow

      root.add(battleGrid, 0, 0, 1, 1)

    end drawBattleground











  end start

end Main