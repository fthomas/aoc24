object Day05 {
  val in = scala.io.Source
    .fromFile("input/input_05.txt")
    .getLines()
    .map(_.trim)
    .toVector
    .dropWhile(_.isEmpty)

  val (rules0, updates0) = in.span(_.contains('|'))

  val rules = rules0
    .map(_.split('|') match { case Array(x, y) => (x.toInt, y.toInt) })
    .toSet

  val updates = updates0.map(_.split(',').toVector.flatMap(_.toIntOption))

  def isOrdered(update: Vector[Int]): Boolean =
    updateToRules(update).subsetOf(rules)

  def updateToRules(update: Vector[Int]): Set[(Int, Int)] =
    update.indices.toSet.flatMap { i =>
      val (before, after0) = update.splitAt(i)
      val after = after0.drop(1)
      val atIndex = update(i)

      println(update)
      println(before)
      println(after)
      println(atIndex)

      val res =
        (before.map(j => j -> atIndex).takeRight(1) ++ after
          .map(j => atIndex -> j)
          .take(1)).toSet
      println(res)
      println("----")
      res
    }

  val ordered = updates.filter(isOrdered).filter(_.nonEmpty)
  println(ordered)
  val middlePages = ordered.map(v => v.apply((v.size) / 2))
  println(middlePages.sum)
}
