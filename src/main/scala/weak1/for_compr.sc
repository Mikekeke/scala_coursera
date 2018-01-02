
abstract class Human {
  def name: String
}
case class Male(name: String) extends Human
case class Female(name: String) extends Human
val population = List(
  Male("Bob"), Male("Tom"), Male("Tod"), Female("Kat"),
  Female("Tanya")
)

for {
  Male(name) <- population // filters implicitly by type
  if name.startsWith("T")
} yield name