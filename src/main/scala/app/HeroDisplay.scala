package app

import game.*
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, Label}
import scalafx.scene.paint.Color.{Black, Blue}

class HeroDisplay(hero: Character):

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
      hero.battle.battleground.lockedSquare.head.lockSwitch()
      hero.battle.battleground.lockedSquare = None
      hero.battle.battleground.movementRadius = None

  heroBox.add(backButton, 0, 1, 1, 1)

  val endTurnButton = new Button("End Turn"):
    onAction = (event) =>
      ()

  heroBox.add(endTurnButton, 1, 1, 1, 1)

  val abilityStatusDisplay = new Label("Ability: Not Used")

  heroBox.add(abilityStatusDisplay, 0, 2, 1, 1)

  def makeAbilityButton(of: Ability) =
    val button = new Button(of.name):
      onAction = (event) =>
        of.use(hero)
    button

  val abilityButtons = hero.getAbilities.map(makeAbilityButton(_))

  heroBox.add(abilityButtons(0), 1, 2, 1, 1)