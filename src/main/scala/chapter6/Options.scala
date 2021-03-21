package chapter6

object Options extends App {

  def readInt(str: String): Option[Int] = str match {
    case str if str matches "-?\\d+" => Some(str.toInt)
    case _ => None
  }

  assert(readInt("20").contains(20))
  assert(readInt("heh").isEmpty)
  assert(readInt("abc").getOrElse(0) == 0)

  val optionPatternMatching = readInt("20") match {
    case Some(number) => number
    case None => 0
  }

  assert(optionPatternMatching == 20)
  println(readInt("20").find(_ == 20))
  println(readInt("20").map(x => x))

  def sum(optionA: Option[Int], optionB: Option[Int]): Option[Int] =
    optionA.flatMap(a => optionB.map(b => a + b))

  assert(sum(readInt("20"), readInt("10")) == Some(30))
  assert(Seq(readInt("1"), readInt("b"), readInt("3")).flatten == Seq(1, 3))


  val optionA = readInt("123")
  val optionB = readInt("234")

  val forOptions = for {
    a <- optionA
    b <- optionB
  } yield a + b

  println(forOptions)

  val q: Option[Int] = None
  assert(q.map(_ + 1).isEmpty)

  def addOptionsFor(aO: Option[Int], bO: Option[Int]): Option[Int] = {
    for {
      a <- aO
      b <- bO
    } yield a + b
  }

  def addOptionsFor(aO: Option[Int], bO: Option[Int], cO: Option[Int]): Option[Int] = {
    for {
      a <- aO
      b <- bO
      c <- cO
    } yield a + b + c
  }

  assert(addOptionsFor(Some(10), Some(20)).contains(30))

  def addOptionsMap(aO: Option[Int], bO: Option[Int]): Option[Int] = {
    aO.flatMap(a => bO.map(b => b + a))
  }

  def addOptionsMap(aO: Option[Int], bO: Option[Int], cO: Option[Int]): Option[Int] = {
    aO.flatMap(a => bO.flatMap(b => cO.map(c => c + b + a)))
  }

  assert(addOptionsMap(Some(10), Some(10)).contains(20))
  assert(addOptionsFor(Some(10), Some(20), Some(10)).contains(40))
  assert(addOptionsMap(Some(10), Some(10), Some(10)).contains(30))

  def divide(a: Int, b: Int): Option[Int] = b match {
    case 0 => None
    case _ => Some(a / b)
  }

  def divideOptions(aO: Option[Int], bO: Option[Int]): Option[Int] = {
    for {
      a <- aO
      b <- bO
      c <- divide(a, b)
    } yield c
  }

  assert(divide(10, 2).contains(5))
  assert(divide(10, 0).isEmpty)
  assert(divideOptions(Some(10), Some(0)).isEmpty)

  def calculator(operand1: String, operator: String, operand2: String): Unit = {
    def toInt(oper: String): Option[Int] = oper match {
      case str if str matches "-?\\d+" => Some(str.toInt)
      case _ => None
    }

    def divide(a: Int, b: Int): Option[Int] = b match {
      case 0 => None
      case _ => Some(a / b)
    }

    val result = for {
      a <- toInt(operand1)
      b <- toInt(operand2)
      ans <- operator match {
        case "+" => Some(a + b)
        case "-" => Some(a - b)
        case "*" => Some(a * b)
        case "/" => divide(a, b)
        case _ => None
      }
    } yield ans

    result match {
      case Some(value) => println(value)
      case None => println("error")
    }
  }

  calculator("10", "+", "10")

}
