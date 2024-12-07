object Day07_02 extends App {
  case class Equation(testValue: BigInt, numbers: List[BigInt])

  sealed trait Eval {
    def eval: BigInt =
      this match {
        case Lit(n)       => n
        case Add(e1, e2)  => e1.eval + e2.eval
        case Mul(e1, e2)  => e1.eval * e2.eval
        case Conc(e1, e2) => BigInt(e1.eval.toString + e2.eval.toString)
      }
  }

  case class Lit(n: BigInt) extends Eval
  case class Add(e1: Eval, e2: Eval) extends Eval
  case class Mul(e1: Eval, e2: Eval) extends Eval
  case class Conc(e1: Eval, e2: Eval) extends Eval

  def readEquation(s: String): Equation = {
    val a1 = s.split(':')
    val testValue = BigInt(a1.head)
    val numbers =
      a1.drop(1).head.split(' ').filter(_.nonEmpty).map(BigInt(_)).toList
    Equation(testValue, numbers)
  }

  def evalsOf(e: Equation): List[Eval] = {
    def go(rest: List[BigInt], acc: List[Eval]): List[Eval] =
      rest match {
        case ::(head, next) =>
          val l = Lit(head)
          val newAcc =
            if (acc.isEmpty) List(l)
            else acc.flatMap(e => List(Add(e, l), Mul(e, l), Conc(e, l)))
          go(next, newAcc)
        case Nil => acc
      }
    go(e.numbers, Nil)
  }

  def firstTrueEval(e: Equation) = {
    val evals = evalsOf(e)
    evals.find(ev => ev.eval == e.testValue)
  }

  val path = os.pwd / "input" / "input_07.txt"
  val lines = os.read.lines(path)

  val equations = lines.map(readEquation)
  val trueEq = equations.flatMap(e => firstTrueEval(e).map(ev => (e, ev)))
  val sum = trueEq.map(_._1.testValue).sum
  println(sum)
}
