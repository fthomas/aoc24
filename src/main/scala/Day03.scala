object Day03 extends App {
  val path = os.pwd / "input" / "input_03.txt"
  val input: String = os.read(path)

  val res = raw"""mul\((\d{1,3}),(\d{1,3})\)""".r
    .findAllMatchIn(input)
    .toVector
    .map { m =>
      val n1 = m.group(1).toInt
      val n2 = m.group(2).toInt
      n1 * n2
    }
    .sum

  println(res)
}
