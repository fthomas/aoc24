object Day02_2 {

  def isSafe(report: Vector[Int]): Boolean = {
    val zipped = report.zip(report.drop(1))
    zipped.forall { case (x1, x2) => (x2 - x1 < 4) && (x2 - x1 > 0) } ||
    zipped.forall { case (x1, x2) => (x1 - x2 < 4) && (x1 - x2 > 0) }
  }

  def variations(report: Vector[Int]): Vector[Vector[Int]] = {
    val v = report.indices.toVector.map { i =>
      val (l1, l2) = report.splitAt(i)
      l1 ++ l2.drop(1)
    }
    println(v)
    v.appended(report)
  }

  val in = scala.io.Source.fromFile("input/input_02.txt").getLines()
  val data =
    in.map(_.split("\\s+").map(_.trim.toInt).toVector).filter(_.nonEmpty)
  println(data.count(r => variations(r).exists(isSafe)))
}
