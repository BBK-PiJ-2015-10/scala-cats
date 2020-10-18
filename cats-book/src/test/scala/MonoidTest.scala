package chapter2

import cats._
import cats.implicits._
import org.scalatest.funsuite.AnyFunSuiteLike

//import org.scalatest.FunSuite

class MonoidTest extends AnyFunSuiteLike{



  test("sum order monoid list") {

    final case class Order(totalCost: Double, quantity: Double)

    implicit val monoid: Monoid[Order] = new Monoid[Order]{
      override def empty: Order = Order(0.0,0.0)
      override def combine(x: Order, y: Order): Order = {
        val totalCost = x.totalCost + y.totalCost
        val quantity = x.quantity + y.quantity
        Order(totalCost,quantity)
      }
    }

    def add[A](items: List[A])(implicit monoid: Monoid[A]): A = items.foldLeft(monoid.empty)(_|+|_)
    def add2[A: Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty)(_|+|_)

    val a: List[Order] = List(Order(1,2),Order(4,5))
    val c = add(a)
    assert(c == Order(5,7))

    val aOptional : List[Option[Order]] = List(Some(Order(1,2)),Some(Order(4,5)))
    val aOptionalCombined = add(aOptional)
    assert(aOptionalCombined == (Some(Order(5,7))))

    val intOptional : List[Option[Int]] = List(Option(1),None)
    val intOptionalCombined = add(intOptional)
    assert(intOptionalCombined == Option(1))


  }

  test(" string monoids") {

    val a = "Scala"
    val b = " with "
    val c = "Cats"

    val d = a |+| b |+| c
    assert(d == "Scala with Cats")

  }


  test("Option monoids") {

    val a = Option(1)
    val b = Option(4)

    val c = a |+| b
    assert(c == Option(5))

  }

  test("Map Monoids") {

    val a = Map("a" -> 1, "b" -> 3)
    val b = Map("b" -> 6, "d" -> 4)
    val c = a |+| b

    assert(c == Map("a" -> 1, "b" -> 9, "d" -> 4))

  }

  test("tupple Monoids") {

    val a = ("hello",123)
    val b = ("brother",456)
    val c = a |+| b

    assert(c == ("hellobrother",579))


  }







}
