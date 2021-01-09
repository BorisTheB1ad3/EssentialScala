package chapter4

import java.time.{LocalDate, LocalDateTime, Period}
import java.time.temporal.ChronoUnit

sealed trait Visitor {
  def id: String
  def createdAt: LocalDateTime

  def age: Long = ChronoUnit.MILLIS.between(createdAt, LocalDateTime.now())
  def older(v1: Visitor, v2: Visitor): Boolean = v1.createdAt.isBefore(v2.createdAt)
}

final class TestUser(val id: String, val createdAt: LocalDateTime) extends Visitor

final case class Anonymous(
  id: String,
  createdAt: LocalDateTime = LocalDateTime.now()
) extends Visitor

final case class User(
  id: String,
  email: String,
  createdAt: LocalDateTime = LocalDateTime.now()
) extends Visitor


object ScalaTraits extends App {
  val anon = Anonymous("1")

  Thread.sleep(1000)

  val user = User("2", "test@example.com")

  println(anon.age)
  println(user.createdAt)

  println(anon.older(this.anon, user))
}
