package Extra

object ExtraPatternMatching extends App {

  case class Positive(vl: Int)

  object Positive {
    def unapply(vl: Int) = {
      if (vl > 0) Some(vl) else None
    }
  }

  assert(
    "No" ==
      (0 match {
        case Positive(_) => "Yes"
        case _ => "No" })
  )

  case class Titlecase(str: String)

  object Titlecase {
    def unapply(str: String) =
      Some(str.split(" ").toList.map(_.capitalize).mkString(" "))
  }

  assert(
    "Sir Lord Doctor David Gurnell" ==
      ("sir lord doctor david gurnell" match {
        case Titlecase(str) => str
      }) )

}
