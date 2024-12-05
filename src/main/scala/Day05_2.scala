object Day05_2 {
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

      val res =
        (before.map(j => j -> atIndex).takeRight(1) ++ after
          .map(j => atIndex -> j)
          .take(1)).toSet
      res
    }

  def order(update: Vector[Int]): Vector[Int] = {
    val matching =
      rules.filter((x, y) => update.contains(x) && update.contains(y))
    val notCorrect = updateToRules(update) -- matching

    println(notCorrect)

    println(matching)

    if (notCorrect.isEmpty) update
    else {
      val x = notCorrect.head._1
      val i_x = update.indexOf(x)
      val (before, after) = update.splitAt(i_x)

      val result = (before :+ after(1) :+ x) ++ after.drop(2)
      println(update)
      println(result)
      println("---")
      order(result)
    }
  }

  val unordered = updates.filterNot(isOrdered).filter(_.nonEmpty)

  val ordered = unordered.map(order)

  val middlePages = ordered.map(v => v.apply((v.size) / 2))
  println(middlePages.sum)
}
