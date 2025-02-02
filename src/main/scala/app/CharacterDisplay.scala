package app

import game.Character
import scalafx.geometry.Insets
import HelpGUI.*
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Label
import scalafx.scene.paint.Color.{Green, Red}

/**
 * Contains the most important information about a character and condenses it to a small window
 * @param of character that the display is based on
 */
class CharacterDisplay(of: Character):

  val characterBox = makeGrid(2, 4)
  characterBox.margin = Insets(3, 3, 3, 3)
  val shifter = "    "

  val portrait = new Canvas():
    width = 50
    height = 50
    graphicsContext2D.drawImage(of.portrait, 0, 0, 50, 50)
  val name = Label(shifter + of.name)
  var armor = Label(shifter + s"Armor: ${of.currentArmor}")
  var statuses = Label(" Statuses: " + of.getStatuses.mkString(", "))
  var healthBar = new Canvas():
    width = 180
    height = 20
    graphicsContext2D.setFill(Red)
    graphicsContext2D.fillRect(0, 10, width.toDouble, height.toDouble)
    graphicsContext2D.setFill(Green)
    graphicsContext2D.fillRect(0, 10, width.toDouble * of.currentHP / of.maxHealth, height.toDouble)
  characterBox.add(portrait, 0, 0, 1, 2)
  characterBox.add(name, 1, 0, 1, 1)
  characterBox.add(armor, 1, 1, 1, 1)
  characterBox.add(statuses, 0, 2, 2, 1)
  characterBox.add(healthBar, 0, 3, 2, 1)

  /**
   * Updates the information of the character based on values in internal logic.
   * Is called every frame
   */
  def update() =
    if characterBox.children.nonEmpty then
      characterBox.children.remove(0, characterBox.children.size - 1)
    armor = Label(shifter + s"Armor: ${of.currentArmor}")
    statuses = Label(" Statuses: " + of.getStatuses.mkString(", "))
    healthBar = new Canvas():
      width = 180
      height = 20
      graphicsContext2D.setFill(Red)
      graphicsContext2D.fillRect(0, 10, width.toDouble, height.toDouble)
      graphicsContext2D.setFill(Green)
      graphicsContext2D.fillRect(0, 10, width.toDouble * of.currentHP / of.maxHealth, height.toDouble)
    characterBox.add(portrait, 0, 0, 1, 2)
    characterBox.add(name, 1, 0, 1, 1)
    characterBox.add(armor, 1, 1, 1, 1)
    characterBox.add(statuses, 0, 2, 2, 1)
    characterBox.add(healthBar, 0, 3, 2, 1)