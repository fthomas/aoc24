import scala.collection.mutable

object Day08_02 {

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

  case class Bounds(leftLower: Pos, rightUpper: Pos) {
    def isWithin(pos: Pos): Boolean =
      pos.x >= leftLower.x && pos.x <= rightUpper.x &&
        pos.y >= leftLower.y && pos.y <= rightUpper.y
  }

  case class Antenna(frequency: Char, pos: Pos)

  def isAntenna(c: Char): Boolean =
    c.isDigit || c.isLower || c.isUpper

  def findMatching(antenna: Antenna, antennas: Vector[Antenna]) =
    antennas.filter(_.frequency == antenna.frequency)

  def antinodeOfFirst(a1: Antenna, a2: Antenna, bounds: Bounds): Set[Pos] =
    if (a1.frequency != a2.frequency || a1.pos == a2.pos) Set.empty
    else {
      val dist = a2.pos.distTo(a1.pos).neg
      LazyList.iterate(a2.pos)(_ + dist).takeWhile(bounds.isWithin).toSet
    }

  def collectAntinodes(antennas: Vector[Antenna], bounds: Bounds): Set[Pos] = {
    val ps = new mutable.HashSet[Pos]
    antennas.foreach { a1 =>
      findMatching(a1, antennas).foreach { a2 =>
        ps.addAll(antinodeOfFirst(a1, a2, bounds))
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
  val bounds = Bounds(Pos(0, 0), Pos(sizeX - 1, sizeY - 1))
  val antinodes = collectAntinodes(antennas, bounds)

  println(antinodes.size)
}
