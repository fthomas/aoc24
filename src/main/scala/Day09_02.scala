object Day09_02 extends App {

  sealed trait Block
  case class Used(id: Int) extends Block
  case object Free extends Block

  case class File(id: Int, length: Int, freeSpace: Int) {
    def toBlocks: Vector[Block] =
      Vector.fill(length)(Used(id)) ++ Vector.fill(freeSpace)(Free)
  }

  def move(files: Vector[File], from: Int, to: Int): Vector[File] = {
    assert(from > to)
    if ((from - to) == 1) {
      val fst = files(to)
      val snd = files(from)
      files
        .updated(to, fst.copy(freeSpace = 0))
        .updated(from, snd.copy(freeSpace = snd.freeSpace + fst.freeSpace))
    } else {
      val (filesBeforeSrc, filesAfterSrc) = files.splitAt(from)
      val (filesBeforeDst, filesAfterDst) = filesBeforeSrc.splitAt(to)

      val src = filesAfterSrc.head
      val dst = filesAfterDst.head
      val dst1 = filesAfterDst.head.copy(freeSpace = 0)
      val dst2 = src.copy(freeSpace = dst.freeSpace - src.length)
      val beforeSrc = filesBeforeSrc.last
      val beforeSrcUpdated =
        beforeSrc.copy(freeSpace = beforeSrc.freeSpace + src.length + src.freeSpace)

      filesBeforeDst ++
        Vector(dst1, dst2) ++
        filesAfterDst.drop(1).dropRight(1) ++
        Vector(beforeSrcUpdated) ++
        filesAfterSrc.drop(1)
    }
  }

  def compact(files: Vector[File]): Vector[File] = {
    def go(currId: Int, files: Vector[File]): Vector[File] = {
      if (currId < 0) files
      else {
        val splitIdx = files.indexWhere(_.id == currId)
        val (before, after) = files.splitAt(splitIdx)
        val file = after.head
        val freeIdx = before.indexWhere(other => other.freeSpace >= file.length)
        if (freeIdx == -1) go(currId - 1, files)
        else {
          val newFiles = move(files, splitIdx, freeIdx)
          go(currId - 1, newFiles)
        }
      }
    }
    val maxId = files.maxBy(_.id).id
    go(maxId, files)
  }

  def checksumOf(files: Vector[File]): BigInt =
    files
      .flatMap(_.toBlocks)
      .zipWithIndex
      .map { case (b, pos) =>
        b match {
          case Used(id) => BigInt(id) * BigInt(pos)
          case Free     => BigInt(0)
        }
      }
      .sum

  val path = os.pwd / "input" / "input_09.txt"
  val diskMap: String = os.read(path).trim
  // val diskMap = "2333133121414131402"

  val layout = diskMap
    .grouped(2)
    .zipWithIndex
    .map { (s, id) =>
      File(
        id = id,
        length = s(0).getNumericValue,
        freeSpace = s.lift(1).map(_.getNumericValue).getOrElse(0)
      )
    }
    .toVector

  val blocks = layout.flatMap(_.toBlocks)
  val compacted = compact(layout)
  val compactedBlocks = compacted.flatMap(_.toBlocks)
  val checksum = checksumOf(compacted)

  println(blocks)
  println("---")
  println(compacted)
  println("---")
  println(compactedBlocks)
  println("---")
  println(checksum)
}
