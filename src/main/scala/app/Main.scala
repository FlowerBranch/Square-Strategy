package app

import app.Main.stage
import game.*
import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.{Background, ColumnConstraints, GridPane, RowConstraints, VBox}
import scalafx.scene.paint.Color.*
import scalafx.event.*
import scalafx.scene.input.*
import scalafx.Includes._

object Main extends JFXApp3:

  def start() =

    val battleground = Battleground(20, 15)
    var pathToDraw: Option[Seq[Seq[SquareCanvas]]] = None

    val root = new GridPane():
      maxWidth = 1600
      maxHeight = 900

    val scene = Scene(parent = root)

    stage = new JFXApp3.PrimaryStage:
      title = "Square Strategy"
      width = 800
      height = 450

    stage.setFullScreen(true)
    stage.scene = scene

    constructBackground()
    drawBattleground()

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

      val battleSquares = battleground.area.map(_.map(i => SquareCanvas(i)))

      battleground.area.foreach(i => i.foreach(j =>
        battleGrid.add(battleSquares(j.x)(j.y).canvas, j.x, j.y)
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

      updateGrid()

      def updateGrid() =
        battleSquares.foreach(_.foreach(i =>
          i.canvas.handleEvent(MouseEvent.Any) {
            (me: MouseEvent) =>
              me.eventType match
                case MouseEvent.MouseMoved =>
                  if !i.square.isHighlighted then
                    i.square.highlightSwitch()
                case MouseEvent.MouseClicked =>
                  if !i.square.isLocked && battleground.lockedSquare.isEmpty then
                    i.square.lockSwitch()
                    battleground.lockedSquare = Some(i.square)
                  else
                    val start = battleground.lockedSquare.head
                    battleground.lockedSquare.head.lockSwitch()
                    battleground.lockedSquare = None
                    pathToDraw = Some(battleSquares.map(_.filter(j =>
                      battleground.squaresAlongPath(start, i.square).exists(k => k.x == j.square.x && k.y == j.square.y)
                    )))
                case _ =>
                  if i.square.isHighlighted then
                    i.square.highlightSwitch()
              drawUpdate()
          }
        ))
      end updateGrid

      def drawUpdate() =
        battleSquares.foreach(_.foreach(i =>
          if i.square.isHighlighted || i.square.isLocked then
            i.canvas.graphicsContext2D.setFill(Red)
            i.canvas.graphicsContext2D.fillRect(0, 0, i.canvas.width.toDouble, i.canvas.height.toDouble)
          else
            i.canvas.graphicsContext2D.setFill(Blue)
            i.canvas.graphicsContext2D.fillRect(0, 0, i.canvas.width.toDouble, i.canvas.height.toDouble)
        ))
        if pathToDraw.isDefined then
          pathToDraw.head.foreach(_.foreach(i =>
            i.canvas.graphicsContext2D.setFill(Grey)
            i.canvas.graphicsContext2D.fillRect(0, 0, i.canvas.width.toDouble, i.canvas.height.toDouble)
          ))

      end drawUpdate

    end drawBattleground

  end start

end Main