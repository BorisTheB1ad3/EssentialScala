package chapter6

object MapsAndSets extends App {
  // Maps
  val example = Map("a" -> 1, "b" -> 2, "c" -> 3)

  assert(example.get("d").isEmpty) // getOrElse for default values
  assert(example.contains("a"))

  val newExampleAdd = example + ("d" -> 4)
  val newExampleRemove = example - ("b", "c")

  // ++ -- -> union/intersection methods

  val unionMaps = Map("e" -> 5) ++ example
  println(example.map(_._2 * 2))


  val people = Set(
    "Alice",
    "Bob",
    "Charlie",
    "Derek",
    "Edith",
    "Fred")
  val ages = Map( "Alice" -> 20, "Bob" -> 30, "Charlie" -> 50, "Derek" -> 40, "Edith" -> 10, "Fred" -> 60)
  val favoriteColors = Map( "Bob" -> "green", "Derek" -> "magenta", "Fred" -> "yellow")
  val favoriteLolcats = Map( "Alice" -> "Long Cat", "Charlie" -> "Ceiling Cat", "Edith" -> "Cloud Cat")


  def favoriteColor(name: String): String =
    favoriteColors.getOrElse(name, "beige")

  def printColors: Unit =
    favoriteColors.foreach(tpl => println(tpl._2))

  def getValueFromMap[A, B](myMap: Map[A, B], valueToGet: A): Option[B] =
    myMap.get(valueToGet)

  def oldestPerson =
    people.foldLeft(Option.empty[String]) { (older, person) =>
      if (ages.getOrElse(person, 0) > older.flatMap(ages.get).getOrElse(0)) Some(person) else older
    }

  assert(favoriteColor("Bob").contains("green"))
  printColors

  println(getValueFromMap[String, String](favoriteLolcats, "Charlie"))
  println(getValueFromMap[String, Int](ages, "Derek"))

  println(oldestPerson)


  def setUnion[A](set1: Set[A], set2: Set[A]): Set[A] =
//    set1.foldLeft(Set.empty[Int])((a, b) => if (set2.contains(b)) a else set2 + b)
    set1.foldLeft(set2)((set, elt) => set + elt)

  def mapUnion[A](map1: Map[A, Int], map2: Map[A, Int]): Map[A, Int] =
    map1.foldLeft(map2)((map, el) => {
      val (key, value1) = el
      map + (key -> (value1 + map.getOrElse(key, 0)))
    })

  println(setUnion[Int](Set(1, 2, 2, 5, 4), Set(2, 3, 4, 5)))
  println(mapUnion[String](Map("a" -> 1, "b" -> 2), Map("a" -> 3, "c" -> 3)))


  assert((1 until 3) == Seq(1, 2))
  assert((1 to 3) == Seq(1, 2, 3))
}
