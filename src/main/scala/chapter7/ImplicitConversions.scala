package chapter7

object ImplicitConversions extends App {
  import scala.language.implicitConversions

  class A

  class B {
    def bar = "This is the best method"
  }

  implicit def aToB(in: A): B = new B()

  println(new A().bar)
}


object ImplicitMethodExer extends App {
  class NewIntMethods(value: Int) {
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

  implicit def IntOps(value: Int) = new NewIntMethods(value)

  IntOps(2).yeah()
}