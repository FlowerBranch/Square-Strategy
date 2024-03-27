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
import scalafx.Includes.*
import scalafx.animation.AnimationTimer

object Main extends JFXApp3:

  def start() =

    val battle = Battle()

    var pathToDraw: Option[Vector[Square]] = None

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

      val battleSquares = battle.battleground.area.map(_.map(i => SquareCanvas(i)))

      battle.battleground.area.foreach(i => i.foreach(j =>
        battleGrid.add(battleSquares(j.x)(j.y).canvas, j.x, j.y)
      ))

      val battleColumn = new ColumnConstraints:
        percentWidth = 1.0 / battle.battleground.width.toDouble * 100.0
      val battleRow = new RowConstraints:
        percentHeight = 1.0 / battle.battleground.height.toDouble * 100.0
      battleGrid.columnConstraints =
        for i <- 0 until battle.battleground.width yield
          battleColumn
      battleGrid.rowConstraints =
        for i <- 0 until battle.battleground.height yield
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
                  if !i.square.isLocked && i.square.hasCharacter && battle.battleground.lockedSquare.isEmpty then
                    i.square.lockSwitch()
                    battle.battleground.lockedSquare = Some(i.square)
                    val start = battle.battleground.lockedSquare.head
                    val agility = i.square.getActor.head.getAgility
                    battle.battleground.movementRadius = Some(battle.battleground.squaresWithinRadius(start, agility))
                  else if battle.battleground.lockedSquare.isDefined then
                    pathToDraw = Some(battle.battleground.squaresAlongPath(i.square, battle.battleground.movementRadius.head))
                    if pathToDraw.head.contains(i.square) then
                      battle.battleground.lockedSquare.head.getActor.head.move(i.square)
                      battle.battleground.lockedSquare.head.lockSwitch()
                      battle.battleground.lockedSquare = None
                      battle.battleground.movementRadius = None
                  else
                    ()
                case _ =>
                  if i.square.isHighlighted then
                    i.square.highlightSwitch()
                drawUpdate()
          }
        ))
      end updateGrid

      def drawUpdate() =

        /*def drawRadius(radius: Vector[(Square, Int)]) =
          battleSquares.map(_.filter(i =>
              radius.map(_._1).exists(j =>
              i.square.x == j.x && i.square.y == j.y
              )))
            .foreach(_.foreach(i =>
            i.canvas.graphicsContext2D.setFill(LightBlue)
            i.canvas.graphicsContext2D.fillRect(0, 0, i.canvas.width.toDouble, i.canvas.height.toDouble)
          ))*/
        def drawPath(path: Vector[Square]) =
          battleSquares.map(_.filter(i =>
              path.exists(j =>
              i.square.x == j.x && i.square.y == j.y
              )))
            .foreach(_.foreach(i =>
            i.canvas.graphicsContext2D.setFill(Grey)
            i.canvas.graphicsContext2D.fillRect(0, 0, i.canvas.width.toDouble, i.canvas.height.toDouble)
          ))

        battleSquares.foreach(_.foreach(i => i.redraw()))
        if pathToDraw.isDefined then drawPath(pathToDraw.head)

      end drawUpdate

    end drawBattleground

  end start

end Main