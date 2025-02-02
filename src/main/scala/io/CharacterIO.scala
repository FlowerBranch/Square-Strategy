package io

import game.*
import java.io.*

/**
 * Class to help differentiate between exceptions mainly for testing purposes
 * @param message Text displayed by exception
 */
class CorruptedCharacterFileException(message: String) extends Exception(message)

/**
 * Object to read character information from external files
 */
object CharacterIO:

  private val allAbilities = Vector(Pyromania, Stab, Burst)

  def readTeam(sourceFile: String, battle: Battle) =

    var fileAsString: String = ""
    try
      val fileIn = FileReader(sourceFile)
      val linesIn = BufferedReader(fileIn)
      try
        var oneLine = linesIn.readLine()
        while oneLine != null do
          fileAsString += oneLine
          oneLine = linesIn.readLine()
      finally
        fileIn.close()
        linesIn.close()
      end try
    catch
      case notFound: FileNotFoundException => throw FileNotFoundException("Couldn't find file")
      case e: IOException => throw IOException("Encountered problem while reading from file")

    fileAsString = fileAsString.filter(_ != ' ')
    val characterStrings = fileAsString.split("---")
    if characterStrings.size != 4 then
      throw CorruptedCharacterFileException("Corrupted character file, could not create 4 characters")

    def createCharacter(basedOn: String): Character =
    //returns one character with all parameters set

      var specString = basedOn
      
      var name = ""
      var picString = ""
      var hp = 0
      var armor = 0
      var agility = 0
      var abilities = Vector[Ability]()

      def defineAttribute(attribute: String, value: String): Unit =
      //takes in an identifier for one character attribute and applies given value to that attribute
        attribute match
          case "NAM" => name = value
          case "PIC" => picString = s"./pics/$value.png"
          case "HPO" => hp = value.toInt
          case "ARM" => armor = value.toInt
          case "AGI" => agility = value.toInt
          case "ABI" =>
             val abilitiesToBe = value.split(",").map(i => allAbilities.find(_.name == i))
             if abilitiesToBe.length == 3 && abilitiesToBe.forall(_.isDefined) then
               abilities = abilitiesToBe.map(_.head).toVector
             else
               throw CorruptedCharacterFileException("Character abilities were incorrectly defined in source file")
          case _     => throw CorruptedCharacterFileException("Couldn't define attribute based on character file")
      end defineAttribute

      while specString.nonEmpty do
        val attribute = specString.take(3)
        specString = specString.drop(3)
        val value = specString.takeWhile(_ != ':')
        specString = specString.drop(value.length + 1)
        defineAttribute(attribute, value)

      Character(battle, name, picString, hp, armor, agility, abilities)

    end createCharacter

    val team = characterStrings.map(createCharacter(_))

    team.toVector