package chapter4

import scala.annotation.tailrec

final case class Broken(broken: Broken)

sealed trait IntList {
  def length: Int = {

    def getLength(l: IntList, acc: Int = 0): Int = l match {
      case End => acc
      case Pair(_, t) => getLength(t, 1 + acc )
    }

    getLength(this)

//    def length: Int =
//      this match {
//        case End => 0
//        case Pair(hd, tl) => 1 + tl.length
//      }

  }

  def product: Int = this match {
    case End => 1
    case Pair(hd, tl) => hd * tl.product
  }

  def double: IntList = this match {
    case End => End
    case Pair(hd, tl) => Pair(hd * 2, tl.double)
  }

}

case object End extends IntList
final case class Pair(head: Int, tail: IntList) extends IntList

// Forest of Trees

sealed trait Tree {
  def sum: Int = {

    def sumTree(t: Tree, acc: Int): Int = t match {
      case Leaf(el) => acc + el
      case Node(left, right) => sumTree(left, 0) + sumTree(right, 0)
    }

    sumTree(this, 0)
  }
}

object TreeOps {
  def double(tree: Tree): Tree = tree match {
    case Leaf(el) => Leaf(el * 2)
    case Node(l, r) => Node(double(l), double(r))
  }
}

final case class Leaf(el: Int) extends Tree
final case class Node(left: Tree, right: Tree) extends Tree

object RecursiveData extends App {

  def sum(list: IntList): Int = list match {
    case End => 0
    case Pair(head, tail) => head + sum(tail)
  }

  @tailrec
  def sum(list: IntList, total: Int = 0): Int =
    list match {
      case End => total
      case Pair(hd, tl) => sum(tl, total + hd)
    }

  val example = Pair(1, Pair(2, Pair(3, End)))

  val d = End
  val c = Pair(3, d)
  val b = Pair(2, c)
  val a = Pair(1, b)

  println(a.head)
  println(a.tail)

  assert(sum(example) == 6)
  assert(sum(example.tail) == 5)
  assert(sum(End) == 0)

  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End.length == 0)

  assert(example.product == 6)
  assert(example.tail.product == 6)
  assert(End.product == 1)

  assert(example.double == Pair(2, Pair(4, Pair(6, End))))
  assert(example.tail.double == Pair(4, Pair(6, End)))
  assert(End.double == End)

  val exampleTree = Node(Leaf(1), Node(Leaf(2), Leaf(2)))
  assert(exampleTree.sum == 5)
  assert(TreeOps.double(exampleTree) == Node(Leaf(2),Node(Leaf(4),Leaf(4))))
}
