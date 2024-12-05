object Day03_02 extends App {
  val path = os.pwd / "input" / "input_03.txt"
  val input: String = os.read(path)

  sealed trait Instruction
  final case class Mul(r: Int) extends Instruction
  case object Do extends Instruction
  case object Dont extends Instruction

  val ints = raw"""mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)""".r
    .findAllMatchIn(input)
    .toVector
    .map { m =>
      if (m.matched.startsWith("mul")) {
        val n1 = m.group(1).toInt
        val n2 = m.group(2).toInt
        Mul(n1 * n2)
      } else if (m.matched.startsWith("don't")) Dont
      else Do
    }

  var multiply = true
  var res = 0
  ints.foreach {
    case Mul(r) if multiply => res = res + r
    case Do                 => multiply = true
    case Dont               => multiply = false
    case _                  => ()
  }
  println(res)
}
