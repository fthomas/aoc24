object Day06 extends App {

  type Grid = Vector[Vector[Char]]

  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction
  case object Up extends Direction
  case object Down extends Direction

  def isSpace(c: Char): Boolean = c == '.'

  def isGuard(c: Char): Option[Direction] =
    if (c == '<') Some(Left)
    else if (c == '>') Some(Right)
    else if (c == '^') Some(Up)
    else if (c == 'v') Some(Down)
    else None

  final case class Pos(x: Int, y: Int)
  final case class Guard(face: Direction, pos: Pos) {
    def turnRight: Guard = {
      face match {
        case Left  => copy(face = Up)
        case Right => copy(face = Down)
        case Up    => copy(face = Right)
        case Down  => copy(face = Left)
      }
    }

    def nextPos: Pos = {
      face match {
        case Left  => Pos(pos.x - 1, pos.y)
        case Right => Pos(pos.x + 1, pos.y)
        case Up    => Pos(pos.x, pos.y - 1)
        case Down  => Pos(pos.x, pos.y + 1)
      }
    }

    def move(grid: Grid): Option[Guard] = {
      val np = nextPos
      grid.lift(np.y).flatMap(_.lift(np.x)) match {
        case Some('#')                                   => turnRight.move(grid)
        case Some(c) if isGuard(c).isDefined || c == '.' => Some(copy(pos = np))
        case _                                           => None
      }
    }
  }

  val path = os.pwd / "input" / "input_06.txt"
  val grid = os.read.lines(path).toVector.map(_.toVector)

  val startingPos = grid
    .map(_.zipWithIndex.collectFirst {
      case (c, x) if isGuard(c).isDefined =>
        (y: Int) => Guard(isGuard(c).get, Pos(x, y))
    })
    .zipWithIndex
    .collectFirst { case (Some(f), y) => f(y) }
    .get

  def isLoop(guard: Guard, g: Grid): Boolean = {
    def go(guard: Guard, visited: Set[Guard]): Boolean = {
      val newVisited = visited + guard
      if (visited.contains(guard)) true
      else
        guard.move(g) match {
          case Some(newGuard) => go(newGuard, newVisited)
          case None           => false
        }
    }
    go(guard, Set.empty)
  }

  val withObstacles = grid.zipWithIndex.flatMap { case (xs, y) =>
    xs.zipWithIndex.flatMap { case (c, x) =>
      if (isSpace(c)) Some(grid.updated(y, xs.updated(x, '#'))) else None
    }
  }

  // 16058 too high

  println(withObstacles.size)
  println(withObstacles.count(g => isLoop(startingPos, g)))
}