package weak2

class Problem(capacity: Vector[Int]) {

  type State = Vector[Int]
  val initState = capacity map (_ => 0)

  trait Action {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Action {
    def change(state: State) = state updated(glass, 0)
  }

  case class Fill(glass: Int) extends Action {
    def change(state: State) = state updated(glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Action {
    def change(state: State) = {
      val amt = state(from) min (capacity(to) - state(to))
      state updated(from, state(from) - amt) updated(to, state(to) + amt)
    }
  }

  val glasses: Range = capacity.indices

  val actions: Seq[Action] =
    glasses.map(Empty) ++ glasses.map(Fill) ++
      (for (from <- glasses; to <- glasses; if from != to) yield Pour(from, to))

  case class Path(steps: List[Action]) {
    def endStateNoPlaceholders(): State = (steps foldRight initState) ((action, state) => action change state)

    def endState() = (steps foldRight initState) (_ change _)

    def extend(a: Action) = Path(a :: steps)

    override def toString: String = steps.reverse mkString "~>" + " --> " + endState()
  }

  val initPath = Path(Nil)

  def from(paths: Set[Path]): Stream[Set[Path]] =
    if(paths.isEmpty) Stream.empty
    else {
      val more: Set[Path] = for {
        path: Path <- paths
        next: Path <- actions map path.extend
      } yield next
      paths #:: from(more)
    }

  val pathSets = from(Set(initPath))


}

object WaterApp extends App {
  val p = new Problem(Vector(4, 9))


}
