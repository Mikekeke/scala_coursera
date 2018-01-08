
abstract class Human {
  val ids: List[Integer]
  def name: String
}
case class Male(name: String, ids: List[Integer]) extends Human {

}
case class Female(name: String, ids: List[Integer]) extends Human
val population = List(
  Male("Bob", List(1,2,3)),
  Male("Tom", List(11,2,2)),
  Male("Tod", List(133,233,333)),
  Female("Kat", List(133,22,33)),
  Female("Tanya", List(133,23,33))
)

for {
  Male(name, _) <- population // filters implicitly by type
  if name.startsWith("T")
} yield name

for {
  human <- population
  ids <- human.ids
  if ids == 2
} yield human

population.flatMap(human => human.ids.withFilter(id => id == 2).map(_ => human))
population.filter(h => h.ids.contains(2))



