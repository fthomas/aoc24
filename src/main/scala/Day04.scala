object Day04 {
  val path = os.pwd / "input" / "input_04.txt"
  val lines = os.read.lines(path).toVector

  val x =
    lines.indices.flatMap(y => lines(0).indices.map(x => countAt(x, y))).sum
  println(x)

  def countAt(x: Int, y: Int): Int =
    at(x, y) match {
      case Some('X') =>
        right(x, y) + left(x, y) + up(x, y) + down(x, y) +
          upRight(x, y) + upLeft(x, y) + downRight(x, y) + downLeft(x, y)
      case _ => 0
    }

  def isMAS(m: Option[Char], a: Option[Char], s: Option[Char]): Int =
    if (m.contains('M') && a.contains('A') && s.contains('S')) 1 else 0

  def at(x: Int, y: Int): Option[Char] =
    lines.lift(y).flatMap(_.lift(x))

  def right(x: Int, y: Int): Int =
    isMAS(at(x + 1, y), at(x + 2, y), at(x + 3, y))

  def left(x: Int, y: Int): Int =
    isMAS(at(x - 1, y), at(x - 2, y), at(x - 3, y))

  def up(x: Int, y: Int): Int =
    isMAS(at(x, y - 1), at(x, y - 2), at(x, y - 3))

  def down(x: Int, y: Int): Int =
    isMAS(at(x, y + 1), at(x, y + 2), at(x, y + 3))

  def upRight(x: Int, y: Int): Int =
    isMAS(at(x + 1, y - 1), at(x + 2, y - 2), at(x + 3, y - 3))

  def downRight(x: Int, y: Int): Int =
    isMAS(at(x + 1, y + 1), at(x + 2, y + 2), at(x + 3, y + 3))

  def downLeft(x: Int, y: Int): Int =
    isMAS(at(x - 1, y + 1), at(x - 2, y + 2), at(x - 3, y + 3))

  def upLeft(x: Int, y: Int): Int =
    isMAS(at(x - 1, y - 1), at(x - 2, y - 2), at(x - 3, y - 3))
}
