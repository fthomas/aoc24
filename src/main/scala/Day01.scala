object Day01 {
  val x = scala.io.Source
    .fromFile("input/input_01.txt")
    .getLines()
    .map(_.split(' ') match {
      case Array(x, _, _, y) => (x.trim.toInt, y.trim.toInt)
      case x =>
        println(x.toList)
        (0, 0)
    })
    .toList

  val firsts = x.map(_._1).sorted
  val seconds = x.map(_._2).sorted

  val sum = firsts.zip(seconds).map { case (x, y) => math.abs(x - y) }.sum

  println(sum)
}
