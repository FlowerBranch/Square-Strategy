package app

import game.*
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.{ColumnConstraints, GridPane, RowConstraints}
import scalafx.scene.paint.Color.Black

/**
 * Contains methods that are more or less essential to the GUI
 */
object HelpGUI:

  /**
   * Returns an evenly spaced grid of given dimensions
   * @param ofDimX width
   * @param ofDimY height
   */
  def makeGrid(ofDimX: Int, ofDimY: Int) =
  
    val grid = GridPane()
      
    val column = new ColumnConstraints:
      percentWidth = 100.0 / ofDimX.toDouble
      
    grid.columnConstraints =
      for i <- 0 until ofDimX yield
        column
    
    val row = new RowConstraints:
      percentHeight = 100.0 / ofDimY.toDouble
  
    grid.rowConstraints =
      for i <- 0 until ofDimY yield
        row
        
    grid
    
  end makeGrid

  /**
   * Creates backround for the app and adds it to the root
   * @param root root node
   */
  def constructBackground(root: GridPane) =
  
    val backdrop = new Canvas():
      width = 1600
      height = 900
      graphicsContext2D.setFill(Black)
      graphicsContext2D.fillRect(0, 0, width.toDouble, height.toDouble)
  
    root.add(backdrop, 0, 0, 2, 1)
  
  end constructBackground

  /**
   * Returns a grid that acts as the root node for the whole GUI
   */
  def makeRoot: GridPane =
  
    val root = new GridPane():
      maxWidth = 1600
      maxHeight = 900
  
    val column0 = new ColumnConstraints:
      percentWidth = 75
    val column1 = new ColumnConstraints:
      percentWidth = 25
  
    root.columnConstraints = Array(column0, column1)
  
    val row0 = new RowConstraints:
      percentHeight = 100
  
    root.rowConstraints = Array(row0)
  
    root
  
  end makeRoot

  /**
   * Returns elements containing information about a given team of characters and adds the to a given parent node
   * @param teamBox parent node
   * @param team vector of characters that elements will be based on
   */
  def makeCharacterBoxes(teamBox: GridPane, team: Vector[Character]): Vector[CharacterDisplay] =
    val characterBoxes = team.map(CharacterDisplay(_))
  
    teamBox.add(characterBoxes(0).characterBox, 0, 0, 1, 1)
    teamBox.add(characterBoxes(1).characterBox, 1, 0, 1, 1)
    teamBox.add(characterBoxes(2).characterBox, 0, 1, 1, 1)
    teamBox.add(characterBoxes(3).characterBox, 1, 1, 1, 1)
    
    characterBoxes
    
  end makeCharacterBoxes