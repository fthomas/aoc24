object Day04_02 {
  val path = os.pwd / "input" / "input_04.txt"
  val lines = os.read.lines(path).toVector

  val x =
    lines.indices.flatMap(y => lines(0).indices.map(x => countAt(x, y))).sum
  println(x)

  def countAt(x: Int, y: Int): Int =
    at(x, y) match {
      case Some('A') =>
        val x1 = upRight(x, y) + downLeft(x, y)
        val x2 = upLeft(x, y) + downRight(x, y)
        if (x1 == 1 && x2 == 1) 1 else 0
      case _ => 0
    }

  def isMAS(m: Option[Char], a: Option[Char], s: Option[Char]): Int =
    if (m.contains('M') && a.contains('A') && s.contains('S')) 1 else 0

  def at(x: Int, y: Int): Option[Char] =
    lines.lift(y).flatMap(_.lift(x))

  def upRight(x: Int, y: Int): Int =
    isMAS(at(x - 1, y + 1), at(x, y), at(x + 1, y - 1))

  def downRight(x: Int, y: Int): Int =
    isMAS(at(x - 1, y - 1), at(x, y), at(x + 1, y + 1))

  def downLeft(x: Int, y: Int): Int =
    isMAS(at(x + 1, y - 1), at(x, y), at(x - 1, y + 1))

  def upLeft(x: Int, y: Int): Int =
    isMAS(at(x + 1, y + 1), at(x, y), at(x - 1, y - 1))
}
