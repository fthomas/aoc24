import scala.collection.mutable

object Day08 {

  case class Dist(dx: Int, dy: Int) {
    def mul(n: Int): Dist = Dist(dx * n, dy * n)
    def neg: Dist = Dist(-dx, -dy)
    def double: Dist = mul(2)
  }

  case class Pos(x: Int, y: Int) {
    def +(d: Dist): Pos =
      Pos(x + d.dx, y + d.dy)

    def distTo(other: Pos): Dist =
      Dist(x - other.x, y - other.y)
  }

  case class Antenna(frequency: Char, pos: Pos)

  def isAntenna(c: Char): Boolean =
    c.isDigit || c.isLower || c.isUpper

  def findMatching(antenna: Antenna, antennas: Vector[Antenna]) =
    antennas.filter(_.frequency == antenna.frequency)

  def antinodeOfFirst(a1: Antenna, a2: Antenna): Option[Pos] =
    if (a1.frequency != a2.frequency || a1.pos == a2.pos) Option.empty
    else {
      val dist = a2.pos.distTo(a1.pos)
      val antinode = a2.pos + dist.neg.double
      Some(antinode)
    }

  def collectAntinodes(antennas: Vector[Antenna]): Set[Pos] = {
    val ps = new mutable.HashSet[Pos]
    antennas.foreach { a1 =>
      findMatching(a1, antennas).foreach { a2 =>
        ps.addAll(antinodeOfFirst(a1, a2))
      }
    }
    ps.toSet
  }

  val path = os.pwd / "input" / "input_08.txt"
  val lines = os.read.lines(path)
  val grid = lines.map(_.toVector).toVector.filter(_.nonEmpty)

  val sizeY = grid.size
  val sizeX = grid(0).size
  val antennas = grid.zipWithIndex.flatMap { case (xs, y) =>
    xs.zipWithIndex.flatMap { case (c, x) =>
      Option.when(isAntenna(c))(Antenna(c, Pos(x, y)))
    }
  }
  val antinodes = collectAntinodes(antennas)
    .filter(pos => pos.x >= 0 && pos.x < sizeX && pos.y >= 0 && pos.y < sizeY)

  println(antinodes.size)
}
