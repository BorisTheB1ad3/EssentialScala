package chapter5

object ModelingDataWithGenericTypes extends App {

  // product type
  final case class Pair[A, B](x: A, b: B)

  val intAndString: Pair[Int, String] = Pair(1, "test")
  assert(intAndString.x == 1)
  assert(intAndString.b == "test")

  val t = (1, "b")

  val res = t match {
    case (a, b) => a + b
  }

  assert(res == "1b")

  // sum type
  sealed trait Sum[A, B] {
    def fold[C](error: A => C, success: B => C): C = this match {
      case Failure(a) => error(a)
      case Success(b) => success(b)
    }

    def map[C](f: B => C): Sum[A, C] = this match {
      case Failure(v) => Failure(v)
      case Success(v) => Success(f(v))
    }

    def flatMap[C](f: B => Sum[A, C]) =
      this match {
        case Failure(v) => Failure(v)
        case Success(v) => f(v)
      }
  }
  final case class Failure[A, B](value: A) extends Sum[A, B]
  final case class Success[A, B](value: B) extends Sum[A, B]

  assert(Left[Int, String](1).value == 1)
  assert(Right[Int, String]("foo").value == "foo")

  val sum: Sum[Int, String] = Success("foo")

  sum match {
    case Failure(x) => println(x.toString)
    case Success(x) => println(x)
  }

  def intOrString(input: Boolean): Sum[Int, String] =
    if(input) {
      Failure[Int, String](123)
    } else {
      Success[Int, String]("abc")
    }

  assert(intOrString(true) == Failure(123))
  assert(intOrString(false) == Success("abc"))

  // Generic optional value
  sealed trait Maybe[A] {
    def fold[B](full: A => B, empty: B): B = this match {
      case Full(value) => full(value)
      case Empty() => empty
    }

    def flatMap[B](fn: A => Maybe[B]): Maybe[B] = this match {
      case Full(v) => fn(v)
      case Empty() => Empty[B]()
    }

//    def map[B](fn: A => B): Maybe[B] = this match {
//      case Full(v) => Full(fn(v))
//      case Empty() => Empty[B]()
//    }

    def map[B](fn: A => B): Maybe[B] = flatMap[B](v => Full(fn(v)))
  }
  case class Empty[A]() extends Maybe[A]
  case class Full[A](value: A) extends Maybe[A]

  val perhapsEmpty: Maybe[Int] = Empty[Int]
  val perhapsFull: Maybe[Int] = Full(1)

  println(perhapsEmpty)
  println(perhapsFull)

  assert(perhapsFull.fold[Int](int => int, 0) == 1)

  val list = List(1, 2, 3)
  println(list.map(x => List(x, -x)))

  val listMaybe: List[Maybe[Int]] = List(Full(3), Full(2), Full(1))
  println(listMaybe.map(maybe => maybe.flatMap[Int] { x => if (x % 2 ==0) Full(x) else Empty()}))


}
