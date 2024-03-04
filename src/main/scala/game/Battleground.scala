package game

class Battleground(val width: Int, val height: Int):

  val area: Seq[Seq[Square]] =
    for i <- 0 until width yield
      for j <- 0 until height yield
        Square(i, j)