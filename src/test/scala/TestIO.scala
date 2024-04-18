import game.*
import io.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Class for testing file reading and extracting information from said files
 */
class TestIO extends AnyFlatSpec with Matchers:
  
  private val battle = Battle()

  "Given a clean sourcefile the readTeam method" should "return correct vector of characters" in {
    assert(CharacterIO.readTeam("./src/test/files/TestCharIO1", battle) == Vector(
      Character(battle, "Sami", "./pics/turquoiseboy.png", 1000, 20, 20, Vector(Pyromania, Stab, Burst)),
      Character(battle, "Rami", "./pics/darkblueboy.png", 500, 10, 40, Vector(Pyromania, Stab, Burst)),
      Character(battle, "Jami", "./pics/greyboy.png", 800, 30, 30, Vector(Pyromania, Stab, Burst)),
      Character(battle, "Monami", "./pics/orangeboy.png", 1200, 40, 10, Vector(Pyromania, Stab, Burst))
    ))
  }

  it should "Throw an Exception given a corrupted sourcefile with too few characters" in {
      intercept[CorruptedCharacterFileException] { CharacterIO.readTeam("./src/test/files/TestCharIO2", battle) }
  }

  it should "Throw an Exception given a corrupted sourcefile with incorrect attribute identification" in {
      intercept[CorruptedCharacterFileException] { CharacterIO.readTeam("./src/test/files/TestCharIO3", battle) }
  }

  it should "Throw an Exception given a corrupted sourcefile with incorrect amount of abilities" in {
      intercept[CorruptedCharacterFileException] { CharacterIO.readTeam("./src/test/files/TestCharIO4", battle) }
  }

  it should "Throw an Exception given a corrupted sourcefile with incorrect names for abilities" in {
      intercept[CorruptedCharacterFileException] { CharacterIO.readTeam("./src/test/files/TestCharIO5", battle) }
  }