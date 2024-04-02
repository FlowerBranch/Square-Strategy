package app

import game.*
import game.Direction.*
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, Label}
import scalafx.scene.paint.Color.{Black, Blue}

class HeroDisplay(hero: Character):

  val directions = Vector(Right, Down, Left, Up).zipWithIndex
  var currentArea = Vector[Square]()

  val heroBox = makeGrid(4, 4)
  heroBox.gridLinesVisible = false

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

  val name = Label(hero.name)
  heroBox.add(name, 1, 0, 1, 1)

  val backButton = new Button("Return"):
    onAction = (event) =>
      currentArea = Vector()
      hero.battle.battleground.lockedSquare.head.lockSwitch()
      hero.battle.battleground.lockedSquare = None
      hero.battle.battleground.movementRadius = None

  heroBox.add(backButton, 0, 1, 1, 1)

  val endTurnButton = new Button("End Turn"):
    onAction = (event) =>
      currentArea = Vector()
      ()

  heroBox.add(endTurnButton, 1, 1, 1, 1)

  val rotateButtonHandler = makeGrid(1, 1)
  rotateButtonHandler.gridLinesVisible = false
  val useButtonHandler = makeGrid(1, 1)
  useButtonHandler.gridLinesVisible = false

  heroBox.add(useButtonHandler, 2, 1, 2, 2)
  heroBox.add(rotateButtonHandler, 0, 2, 1, 1)

  def makeAbilityButton(of: Ability) =
    val button = new Button(of.name):
      onAction = (event) =>
        if rotateButtonHandler.children.nonEmpty then
          rotateButtonHandler.children.remove(0, rotateButtonHandler.children.size - 1)
        var currentDirection = directions.head
        currentArea = of.areaOfEffect(hero, currentDirection._1)
        val rotateButton = new Button("Rotate"):
          onAction = (event) =>
            if currentDirection._2 < 3 then
              currentDirection = directions.find(p => p._2 == currentDirection._2 + 1).head
            else
              currentDirection = directions.head
            currentArea = of.areaOfEffect(hero, currentDirection._1)
        rotateButtonHandler.add(rotateButton, 0, 0, 1, 1)
    button

  val abilityButtons = hero.getAbilities.map(makeAbilityButton(_))

  heroBox.add(abilityButtons(0), 1, 2, 1, 1)