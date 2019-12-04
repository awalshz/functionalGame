import streams._

abstract class Level extends Solver with StringParserTerrain

object Level0 extends Level {
  val level =
    """------
      |--ST--
      |--oo--
      |--oo--
      |------""".stripMargin
}

val obj = Level0

//obj.startPos
case class Pos(row: Int, col: Int) {
  /** The position obtained by changing the `row` coordinate by `d` */
  def deltaRow(d: Int): Pos = copy(row = row + d)

  /** The position obtained by changing the `col` coordinate by `d` */
  def deltaCol(d: Int): Pos = copy(col = col + d)
}

def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
  val row = levelVector.indexWhere(_ contains c)
  val col = levelVector(row).indexWhere(_ == c)
  Pos(row, col)
}

private lazy val vector: Vector[Vector[Char]] =
  Vector(obj.level.split("\n").map(str => Vector(str: _*)): _*)

lazy val startPos: Pos = findChar('S', vector)
//obj.goal
//obj.terrain
//obj.startBlock
//val b = obj.Block(obj.Pos(2,2), obj.Pos(2, 3))
//b.neighbors
//b.legalNeighbors
//b.isStanding
//b.isLegal