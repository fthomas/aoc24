object Day02 {

  def isSafe(report: List[Int]): Boolean = {
    val zipped = report.zip(report.drop(1))
    println(zipped)
    zipped.forall { case (x1, x2) => (x2 - x1 < 4) && (x2 - x1 > 0) } ||
    zipped.forall { case (x1, x2) => (x1 - x2 < 4) && (x1 - x2 > 0) }
  }

  val in = scala.io.Source.fromFile("input/input_02.txt").getLines()
  val data = in.map(_.split("\\s+").map(_.trim.toInt).toList).filter(_.nonEmpty)
  println(data.count(isSafe))
}
