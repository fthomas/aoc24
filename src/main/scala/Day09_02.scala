object Day09_02 extends App {

  sealed trait Block

  case class Used(id: Int, length: Int) extends Block

  case class Free(length: Int) extends Block

  case class Dense(id: Int, length: Int, freeSpace: Int) {
    def toBlocks: Vector[Block] =
      Vector(Used(id, length), Free(freeSpace))
  }

  def compact(blocks: Vector[Block]): Vector[Block] = {
    def go(currId: Int, blocks: Vector[Block]): Vector[Block] =
      if (currId < 0) blocks
      else {
        val idx = blocks.indexWhere {
          case Used(id, _) => id == currId
          case Free(_)     => false
        }
        if (idx == -1) blocks
        else {
          val used = blocks(idx).asInstanceOf[Used]
          val (beforeUsed, afterUsed) = blocks.splitAt(idx)
          val freeIdx = beforeUsed.indexWhere {
            case Free(length) => length >= used.length
            case _            => false
          }
          if (freeIdx == -1) go(currId - 1, blocks)
          else {
            val free = beforeUsed(freeIdx).asInstanceOf[Free]
            val (beforeFree, afterFree) = beforeUsed.splitAt(freeIdx)
            val nextBlock = afterFree.lift(1)
            val replace =
              if (free.length > used.length) {
                nextBlock match {
                  case Some(next @ Used(_, _)) =>
                    Vector(used, Free(free.length - used.length), next)
                  case Some(Free(length)) =>
                    Vector(used, Free(length + free.length - used.length))
                  case None =>
                    Vector(used, Free(free.length - used.length))
                }
              } else Vector(used, nextBlock)

            val newFreeWhereUsed = {
              (afterFree.drop(2).lastOption, afterUsed.lift(1)) match
                case (Some(first: Used), Some(next: Used)) =>
                  Vector(first, Free(used.length), next)
                case (Some(Free(l1)), Some(Free(l))) =>
                  Vector(Free(l1 + l + used.length))
                case (Some(b), None) => Vector(b)
                case _               => Vector.empty
            }

            val newBlocks =
              beforeFree ++ replace ++ afterFree
                .drop(2)
                .dropRight(1) ++ newFreeWhereUsed ++
                afterUsed.drop(2)
            go(currId - 1, newBlocks)
          }
        }
      }

    val maxId = blocks.map {
      case Used(id, _) => id
      case Free(_)     => -1
    }.max
    go(maxId, blocks)
  }

  def checksum(blocks: Vector[Block]): BigInt =
    blocks
      .flatMap {
        case used @ Used(id, length) => Vector.fill(length)(Used(id, 1))
        case Free(length)            => Vector.fill(length)(Free(1))
      }
      .zipWithIndex
      .map { case (b, pos) =>
        b match
          case Used(id, _) => BigInt(id) * pos
          case Free(_)     => BigInt(0)
      }
      .sum

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
  println("---")
  println(compacted)
  println(checksum(compacted))
}
