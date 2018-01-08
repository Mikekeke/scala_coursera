package weak1

trait Generator[+T] {
  self =>
  def generate: T

  def map[U](f: T => U) = new Generator[U] {
    override def generate: U = f(self.generate)
  }

  def flatMap[U](f: T => Generator[U]) = new Generator[U] {
    override def generate: U = f(self.generate).generate
  }
}

object RngApp extends App {
  def ints: Generator[Int] = new Generator[Int] {
    val rng = scala.util.Random
    override def generate: Int = rng.nextInt()
  }

  def bools = ints.map(_>0)
  def bools2 = for(x <- ints) yield x > 0

  def single[T](x: T) = new Generator[T] {
    override def generate: T = x
  }

//  lists gen
  def emptyList[T] = single(Nil)

  def list[T]: Generator[List[Int]] = for {
    head <- ints
    tail <- lists
  } yield head :: tail

  def list2[T] = ints.flatMap(head => lists.map(tail => head :: tail))

  def lists: Generator[List[Int]] = for {
    isEmpty <- bools
    list <- if (isEmpty) emptyList else list
  } yield list

  def lists2: Generator[List[Int]] = bools.flatMap(isEmpty => (if (isEmpty) emptyList else list).map(list => list))


//  trees gen
  trait Tree
  case class Node(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  def leafs = ints map Leaf
  def nodes = for {
    l1 <- trees
    l2 <- trees
  } yield Node(l1, l2)

  def trees: Generator[Tree] = for {
      isNode <- bools
      tree <- if (isNode) nodes else leafs
    } yield tree

  1 to 10 foreach(_=> println(trees.generate))

}
