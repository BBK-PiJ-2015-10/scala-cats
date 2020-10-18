package chapter1

import cats.implicits._
import cats._
//import cats.instances.int._
//import cats.implicits._

object Chapter1 extends App {

  //println("Hello" |+| "Cats!")

  val cat = Cat("Rufus",8,"beige")

  // using the printable object
  import PrintableInstances.catPrintable

  Printable.print(cat)

  // using the printable syntax
  import PrintableSyntax._

  cat.print

  val showInt = Show.apply[Int]
  val showString = Show.apply[String]

  val intsAsString: String = showInt.show(123)
  val stringAsString : String = showString.show("abc")

  val showInt2 = 123.show
  val showString2 = "abc".show

  import java.util.Date

  //implicit val dateShow : Show[Date] =
   //new Show[Date] {
     //override def show(t: Date): String = s"${t.getTime}ms since the epoch."
   //}



  implicit val dateShow2: Show[Date] = Show.show(date => s"${date.getTime}ms since the epoch.")

  new Date().show

  //using cats's show
  implicit val catShow : Show[Cat] = Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")

  println(cat.show)

  val eqInt = Eq[Int]

  eqInt.eqv(123,123)

  123.eqv(456)

  123 === 456
  123 =!= 457

  (Some(1) : Option[Int]) === (None: Option[Int])

  Option(1) === Option.empty[Int]

  1.some === none[Int]
  1.some =!= none[Int]


  implicit val dateEq: Eq[Date] = Eq.instance[Date] {(date1, date2) => date1.getTime === date2.getTime}

  val x = new Date()
  val y = new Date()

  x === x
  x === y

  val cat1 = Cat("Garfield",   38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  implicit val catEq: Eq[Cat] = Eq.instance[Cat] {(cat1,cat2) => {
    val equalName = cat1.name === cat2.name
    val equalAge = cat1.age === cat2.age
    val equalColor = cat1.color === cat2.color
    equalName & equalAge & equalColor
  }}


  println(cat1 === cat1)
  println(cat1 === cat2)
  println(optionCat1 === optionCat2)
  println(optionCat2 === none[Cat])









}
