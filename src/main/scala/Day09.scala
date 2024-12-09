object Day09 {

  sealed trait Block
  case class Used(id: Int) extends Block
  case object Free extends Block

  case class Dense(id: Int, length: Int, freeSpace: Int) {
    def toBlocks: Vector[Block] =
      Vector.fill(length)(Used(id)) ++ Vector.fill(freeSpace)(Free)
  }

  def compact(blocks: Vector[Block]): Vector[Block] = {
    val idx = blocks.indexWhere(_ == Free)
    if (idx == -1) blocks
    else {
      blocks.last match
        case used @ Used(_) => compact(blocks.dropRight(1).updated(idx, used))
        case Free           => compact(blocks.dropRight(1))
    }
  }

  def checksum(blocks: Vector[Block]): BigInt =
    blocks.zipWithIndex.map { case (b, pos) =>
      b match
        case Used(id) => BigInt(id) * pos
        case Free     => BigInt(0)
    }.sum

  val path = os.pwd / "input" / "input_09.txt"
  val diskMap: String = os.read(path).trim
  // val diskMap = "2333133121414131402"

  val layout = diskMap
    .grouped(2)
    .zipWithIndex
    .map { (s, id) =>
      Dense(
        id = id,
        length = s(0).getNumericValue,
        freeSpace = s.lift(1).map(_.getNumericValue).getOrElse(0)
      )
    }
    .toVector
  val blocks = layout.flatMap(_.toBlocks)
  val compacted = compact(blocks)

  println(blocks)
  println(compacted)
  println(checksum(compacted))
}
