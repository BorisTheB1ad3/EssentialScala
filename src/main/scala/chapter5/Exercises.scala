package chapter5

sealed trait NewTree[A] {
  def fold[B](node: (B, B) => B, leaf: A => B): B
}

final case class Leaf[A](el: A) extends NewTree[A] {
  def fold[B](node: (B, B) => B, leaf: A => B): B = leaf(el)
}

final case class Node[A](left: NewTree[A], right: NewTree[A]) extends NewTree[A] {
  def fold[B](node: (B, B) => B, leaf: A => B): B =
    node(left.fold(node, leaf), right.fold(node, leaf))
}


object Exercises extends App {
  val tree: NewTree[String] =
    Node(Node(Leaf("To"), Leaf("iterate")),
      Node(Node(Leaf("is"), Leaf("human,")),
        Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))))

  val myTree: NewTree[String] =
    Node(Leaf("thx"), Node(Leaf("to"), Leaf("you")))

  println(tree.fold[String]((a, b) => a + " " + b, str => str))
}
