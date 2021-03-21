package chapter7

import chapter7.CreatingTypeClasses.Person

object TypeEnrichment extends App {
  /* type enrichment -> interfaces acts like methods ^^ */

  /* implicit classes allows to add new functionality to an existing class
    without editing its source code */


  implicit class ExtraStringMethods(str: String) {
    val vowels = Seq('a', 'e', 'i', 'o', 'u')

    def numberOfVowels = str.toList.count(vowels contains _)
  }

  assert("a string".numberOfVowels == 2)


  /* Combination of type class and type enrichment */

  trait HtmlWriter[A] {
    def write(data: A): String
  }

  object HtmlWriter{
    def apply[A](implicit writer: HtmlWriter[A]): HtmlWriter[A] = writer
  }

  implicit object HtmlWriterObject extends HtmlWriter[Person] {
    def write(a: Person): String = a.email + "    " + a.name
  }

  implicit class HtmlOps[T](data: T) {
    def write(implicit writer: HtmlWriter[T]) = writer.write(data)
  }

  println(HtmlWriter[Person].write(Person("John", "test@mail.ru")))
  println(Person("John", "test@mail.ru").write)
}

object TypeClassEnrichment extends App {

  trait Equal[A] {
    def equal(v1: A, v2: A): Boolean
    def notEqual(v1: A, v2: A): Boolean
  }

  object Equal {
    def apply[A](implicit instance: Equal[A]): Equal[A] = instance

    implicit class Equalizier[A](in: A) {
      def ===(other: A)(implicit equal: Equal[A]): Boolean = {
        equal.equal(in, other)
      }

      def =!=!=(other: A)(implicit equal: Equal[A]): Boolean = {
        equal.notEqual(in, other)
      }
    }
  }

  import Equal._
  implicit val caseInsensitiveEquals = new Equal[String] {
    override def equal(v1: String, v2: String): Boolean = v1.toLowerCase == v2.toLowerCase

    override def notEqual(v1: String, v2: String): Boolean = v1.toLowerCase != v1.toLowerCase
  }

  implicit val IntNotEquals = new Equal[Int] {
    override def equal(v1: Int, v2: Int): Boolean = v1 == v2

    override def notEqual(v1: Int, v2: Int): Boolean = v1 != v2
  }



  assert("abc".===("ABC"))
  assert(2.=!=!=(3))

  assert(Equal[Int].equal(1, 1))

  implicit class NewIntMethods(value: Int) {
    def yeah() = {
      for (i <- (1 to value)) {
        println("Oh, yeah!")
      }
    }

    def times(f: Int => Unit) = {
      for (i <- 1 to value) {
        f(i)
      }
    }
  }

  4.yeah()
  println("-----")
  3.yeah()
  println("-----")
  (-1).yeah()
  println("-----")


  3.times(i => println(s"Look - it's the number $i!"))
}

object ContextBound extends App {

  trait TextWriter[A] {
    def getBetterText(in: A): String
  }

  implicit val textWriterString: TextWriter[String] = new TextWriter[String] {
    override def getBetterText(a: String): String = a.toLowerCase
  }

  implicit class textWriterStringEnrichment[A](in: A) {
    def getBetterText(implicit writer: TextWriter[A]): String = writer.getBetterText(in)
  }

  def betterText[A : TextWriter](body: A): String = {
    body.getBetterText
  }

  println("zZZZzzzZZzz".getBetterText)

}

object ImplicitlyTest extends App {

  implicit val b = 2
  val a = implicitly[Int]

  assert(a == b)

}