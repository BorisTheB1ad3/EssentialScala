package chapter5


object Functions extends App {

  // val ... = () => ... - function literal syntax
  val sayHi = () => "Hi!"
  assert(sayHi() == "Hi!")

  val add1 = (x: Int) => (x + 1): Int
  assert(add1(10) == 11)

  val sum = (x: Int, y: Int) => x + y : Int // we can declare result type here
  assert(sum(10, 10) == 20)

  sealed trait IntList {
    def fold[A](end: A, f: (Int, A) => A): A =
      this match {
        case End => end
        case Pair(hd, tl) => f(hd, tl.fold(end, f))
      }

    def length: Int = fold[Int](0, (_, tl) => 1 + tl)
    def product: Int = fold[Int](1, (el, tl) => el * tl)
    def sum: Int = fold[Int](0, (el, tl) => el + tl)
    def double: IntList = fold[IntList](End, (hd, tl) => Pair(hd * 2, tl))

  }
  case object End extends IntList
  final case class Pair(head: Int, tail: IntList) extends IntList

  val example = Pair(1, Pair(2, Pair(3, End)))
  assert(example.length == 3)
  assert(example.product == 6)
  assert(example.sum == 6)
  assert(example.double == Pair(2, Pair(4, Pair(6, End))))
}
