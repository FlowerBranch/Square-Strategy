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
import scalafx.scene.control.*

object Main extends JFXApp3:

  def start() =

    val battle = Battle()
    val battleSquares = battle.battleground.area.map(_.map(i => SquareCanvas(i)))

    val battleGrid = makeGrid(battle.battleground.width, battle.battleground.height)
    battleGrid.margin = Insets(3, 3, 3, 3)

    battle.battleground.area.foreach(i => i.foreach(j =>
      battleGrid.add(battleSquares(j.x)(j.y).canvas, j.x, j.y)
    ))

    var currentHero: Option[HeroDisplay] = None

    val root = makeRoot

    constructBackground(root)

    val gameScene = Scene(parent = root)

    val infoBox = makeGrid(1, 4)
    infoBox.background = Background.fill(White)

    val heroBoxController = makeGrid(1, 1)

    val playerBox = makeGrid(2, 2)
    val goodGuyBoxes = makeCharacterBoxes(playerBox, battle.playerTeam)

    val enemyBox = makeGrid(2, 2)
    val badGuyBoxes = makeCharacterBoxes(enemyBox, battle.enemyTeam)

    infoBox.add(heroBoxController, 0, 1, 1, 1)
    infoBox.add(playerBox, 0, 0, 1, 1)
    infoBox.add(enemyBox, 0, 3, 1, 1)

    root.add(infoBox, 1, 0, 1, 1)
    root.add(battleGrid, 0, 0, 1, 2)

    stage = new JFXApp3.PrimaryStage:
      title = "Square Strategy"
      width = 800
      height = 450
      fullScreen = true
      scene = gameScene

    battleEvents()
    battle.play(drawUpdate())

    def battleEvents() =
      battleSquares.foreach(_.foreach(i =>
        i.canvas.handleEvent(MouseEvent.Any) {
          (me: MouseEvent) =>
            me.eventType match

              case MouseEvent.MouseMoved =>
                if !i.square.isHighlighted then
                  i.square.highlightSwitch()

              case MouseEvent.MouseClicked =>

                  if !i.square.isLocked
                    && battle.playerTeam.exists(j => Some(j) == i.square.getActor && !j.turnIsOver)
                    && battle.battleground.lockedSquare.isEmpty then
                    i.square.lockSwitch()
                    battle.battleground.lockedSquare = Some(i.square)
                    val start = battle.battleground.lockedSquare.head
                    val agility = i.square.getActor.head.getAgility
                    battle.battleground.movementRadius = Some(battle.battleground.squaresWithinRadius(start, agility))
                  else if battle.battleground.lockedSquare.isDefined then
                    battle.battleground.lockedSquare.head.getActor.head.onTheMove = Some(battle.battleground.squaresAlongPath(i.square, battle.battleground.movementRadius.head))
                    if battle.battleground.lockedSquare.head.getActor.head.onTheMove.head.contains(i.square) then
                      battle.battleground.lockedSquare.head.lockSwitch()
                      battle.battleground.lockedSquare = None
                      battle.battleground.movementRadius = None
                  else
                    ()

              case _ =>
                if i.square.isHighlighted then
                  i.square.highlightSwitch()
        }
      ))
    end battleEvents

    def drawUpdate() =

      def updateSideBar() =
        goodGuyBoxes.foreach(_.update())
        badGuyBoxes.foreach(_.update())
        if battle.battleground.lockedSquare.isDefined then
          if currentHero.isEmpty then
            currentHero = Some(HeroDisplay(battle.playerTeam.find(_.location == battle.battleground.lockedSquare).head))
            if heroBoxController.children.nonEmpty then
              heroBoxController.children.remove(heroBoxController.children.size - 1)
            heroBoxController.add(currentHero.head.heroBox, 0, 0, 1, 1)
        else
          if heroBoxController.children.nonEmpty then
            heroBoxController.children.remove(heroBoxController.children.size - 1)
          currentHero = None

      def updateAbilityAoE() =
        if currentHero.isDefined then
          battleSquares.flatten.filter(i => currentHero.head.currentArea.contains(i.square)).foreach(i =>
            i.canvas.graphicsContext2D.setFill(Red)
            i.canvas.graphicsContext2D.fillRect(0, 0, 10, 10)
          )
        else
          ()

      updateSideBar()
      battleSquares.foreach(_.foreach(i => i.redraw()))
      updateAbilityAoE()

    end drawUpdate

  end start

end Main