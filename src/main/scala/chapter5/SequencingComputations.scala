package chapter5

final case class Box[A](value: A)

sealed trait Result[A]
case class Success[A](result: A) extends Result[A]
case class Failure[A](reason: String) extends Result[A]

sealed trait LinkedList[A] {
  def map[B](fn: A => B): LinkedList[B] = this match {
    case Pair(hd, tl) => Pair(fn(hd), tl.map(fn))
    case End() => End[B]()
  }

  def fold[B](end: B)(pair: (A, B) => B): B =
    this match {
      case End() => end
      case Pair(hd, tl) => pair(hd, tl.fold(end)(pair))
    }

  def length: Int = fold(0)((_, tl) => 1 + tl)

  def contains(value: A): Boolean = this match {
    case End() => false
    case Pair(h, t) => if (h equals value) true else t.contains(value)
  }

  def apply(n: Int): Result[A] = this match {
    case End() => Failure("Index out of bounds")
    case Pair(h, t) => if (n == 0) Success(h) else t.apply(n - 1)
  }
}
final case class End[A]() extends LinkedList[A]
final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]


object SequencingComputations extends App {
  println(Box(2))
  println(Box("test"))

  def generic[A](in: A): A = in

  println(generic[String]("foo"))
  println(generic(1))

  val example = Pair(1, Pair(2, Pair(3, End())))

  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End().length == 0)

  assert(example.contains(3))
  assert(!example.contains(4))
  assert(!End().contains(0))

  assert(example(0) == Success(1))
  assert(example(1) == Success(2))
  assert(example(2) == Success(3))
  assert(example(3) == Failure("Index out of bounds"))

  println(example.fold[Int](0)((head, tail) => head + tail))
  // if End, than 0
  // else if Pair(1, Pair(2,...))) => pair(1, Pair(2,...).fold(0, h + t) => pair(2, Pair(3,...).fold(0, h + t))

  val testList = List(1, 2, 3)

  println(Set(1, 2, 3).==(Set(1, 2, 3)))
  println(testList.lift(4))
  println(testList.indices)
  println(testList.zipWithIndex)
  println(testList.iterator.find(_ == 4))
  println(testList.iterator.forall(_ < 4))

  val p = testList map {
    case el if el > 4 => true
    case _ => false
  }

  println(p)

  sealed trait Choice
  case object GoodChoice extends Choice
  case object BadChoice extends Choice

  println(testList.partition(_ > 2))
  println(List(GoodChoice, BadChoice, BadChoice).partition(_ == BadChoice))

  println(testList.view.map(_ * 2))

  println(example.map(_ * 2).map(_ + 1).map(_ / 3))
}
