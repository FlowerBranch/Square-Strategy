package app

import app.Main.stage
import game.*
import game.Direction.*
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, Button, ButtonType, Label}
import scalafx.scene.paint.Color.{Black, Blue, Red}
import scalafx.scene.shape.Circle

class HeroDisplay(hero: Character):

  val directions = Vector(Right, Down, Left, Up).zipWithIndex
  var currentArea = Vector[Square]()

  def emptyTime() =
    currentArea = Vector()
    hero.battle.battleground.lockedSquare.head.lockSwitch()
    hero.battle.battleground.lockedSquare = None
    hero.battle.battleground.movementRadius = None

  val heroBox = makeGrid(4, 4)

  val portrait = new Canvas():
    width = 50
    height = 50
    if hero.battle.playerTeam.contains(hero) then
      graphicsContext2D.setFill(Blue)
      graphicsContext2D.fillOval(0, 10, width.toDouble, height.toDouble - 10)
    else
      graphicsContext2D.setFill(Black)
      graphicsContext2D.fillOval(0, 10, width.toDouble, height.toDouble - 10)

  heroBox.add(portrait, 0, 0, 1, 1)

  val hasBeenUsedAlert = new Alert(AlertType.Confirmation):
    initOwner(stage)
    title = "OOF!"
    headerText = "This character has already used its ability this turn"
    graphic = Circle(20, Red)
    buttonTypes = Array(ButtonType.OK)

  val name = Label(hero.name)
  heroBox.add(name, 1, 0, 1, 1)

  val backButton = new Button("Return"):
    onAction = (event) =>
      emptyTime()

  heroBox.add(backButton, 0, 1, 1, 1)

  val endTurnButton = new Button("End Turn"):
    onAction = (event) =>
      emptyTime()
      hero.turnEnded = true

  heroBox.add(endTurnButton, 1, 1, 1, 1)

  val rotateButtonHandler = makeGrid(1, 1)
  val useButtonHandler = makeGrid(1, 1)

  heroBox.add(useButtonHandler, 2, 1, 2, 2)
  heroBox.add(rotateButtonHandler, 1, 3, 1, 1)

  def makeAbilityButton(of: Ability) =
    val button = new Button(of.name):
      onAction = (event) =>
        if rotateButtonHandler.children.nonEmpty then
          rotateButtonHandler.children.remove(0, rotateButtonHandler.children.size - 1)
        if useButtonHandler.children.nonEmpty then
          useButtonHandler.children.remove(0, useButtonHandler.children.size - 1)

        var currentDirection = directions.head
        currentArea = of.areaOfEffect(hero.location.head, currentDirection._1)

        val rotateButton = new Button("Rotate"):
          onAction = (event) =>
            if currentDirection._2 < 3 then
              currentDirection = directions.find(p => p._2 == currentDirection._2 + 1).head
            else
              currentDirection = directions.head
            currentArea = of.areaOfEffect(hero.location.head, currentDirection._1)

        val useButton = new Button("Use Ability"):
          onAction = (event) =>
            if !hero.abilityUsed then
              of.use(hero, currentDirection._1)
              hero.abilityUsed = true
              emptyTime()
            else
              hasBeenUsedAlert.show()

        rotateButtonHandler.add(rotateButton, 0, 0, 1, 1)
        useButtonHandler.add(useButton, 0, 0, 1, 1)

    button

  val abilityButtons = hero.getAbilities.map(makeAbilityButton(_))

  heroBox.add(abilityButtons(0), 0, 2, 1, 1)
  heroBox.add(abilityButtons(1), 1, 2, 1, 1)
  heroBox.add(abilityButtons(2), 0, 3, 1, 1)