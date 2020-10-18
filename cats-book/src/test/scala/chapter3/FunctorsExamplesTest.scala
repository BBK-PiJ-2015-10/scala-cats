package chapter3

import cats.Functor
import cats.instances.function._
import cats.syntax.functor._
import cats.Functor._
import cats.instances.list._
import cats.instances.option._
import org.scalatest.funsuite.AnyFunSuiteLike
import cats.Contravariant
//import cats.Invariant.ops.toAllInvariantOps
import cats.InvariantSemigroupal.ops.toAllInvariantSemigroupalOps
import cats.Show
import cats.instances.string._
import cats.syntax.contravariant._
import cats.Monoid
import cats.instances.string._
import cats.syntax.invariant._
import cats.syntax.semigroup._
import cats.Functor
import cats.instances.function._
import cats.syntax.functor._

class FunctorsExamplesTest extends AnyFunSuiteLike {

  test("simple functors with functor.map"){

    val func1: Int => Double = (x: Int) => x.toDouble

    val func2: Double => Double = (y: Double) => y*2

    // using maps
    val result = (func1 map func2)(1)
    assert(2.0 == result)

    //using andThen
    val result2 = (func1 andThen func2)(1)
    assert(2.0 == result2)

    //fog way
    val result3 = func2(func1(1))
    assert(2.0 == result3)

    val func =
      ((x: Int) => x.toDouble)
        .map(x => x + 1)
        .map(x => x * 2)
        .map(x => s"${x}!")

    val result4 = func(123)
    assert("248.0!" == result4)

  }

  test("list and options functors examples"){

    val list1 = List(1,2,3)
    val list2 = Functor[List].map(list1)(_ * 2)
    assert(List(2,4,6) == list2)

    val option1 = Option(123)
    val option2 = Functor[Option].map(option1)(_.toString)
    assert(Some("123") == option2)


  }

  test("lift example") {

    val func = (x: Int) => x + 1
    val liftedFunc = Functor[Option].lift(func)
    liftedFunc(Option(2))
    assert(Some(3) == liftedFunc(Some(2)))

  }

  test("as method example") {

    val list1 = List(1,2,3)
    val result = Functor[List].as(list1,"As")
    assert(List("As","As","As") == result)

  }

  test("functors syntax"){

    val func1 = (a: Int) => a + 1
    val func2 = (a: Int) => a * 2
    val func3 = (a: Int) => s"${a}!"
    val func4 = func1.map(func2).map(func3)

    assert("248!" == func4(123))

    def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] = start.map(n => n + 1 * 2)

    assert(Some(22) == doMath(Option(20)))
    assert(doMath(List(1,2,3)) == List(3,4,5))

  }

  test("custom syntax"){

    final case class Box[A](value: A)
    val box = Box[Int](123)


    // you need an implicit functor for the next box.map(value => value + 1)

  }

  test("Branching out with Functors "){

    sealed trait Tree[+A]
    final case class Branch[A](left: Tree[A], right:Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]

    object Tree {
      def branch[A](left: Tree[A],right:Tree[A]): Tree[A] = Branch(left,right)
      def leaf[A](value: A): Tree[A] = Leaf(value)
    }

    implicit val treeFunctor: Functor[Tree] =
      new Functor[Tree] {
        override def map[A, B](value: Tree[A])(func: A => B): Tree[B] =
          value match {
            case Leaf(value) => Leaf(func(value))
            case Branch(left,right) => Branch(map(left)(func),map(right)(func))
          }
      }

    assert(Tree.leaf(100).map(_ * 2) == Leaf(200))
    val result2 = Tree.branch(Tree.leaf(10),Tree.leaf(20)).map(_ * 2)
    assert(result2 == Tree.branch(Tree.leaf(20),Tree.leaf(40)))

    //Left on page 60 out of 74

  }

  test("Contramap example"){

    final case class Box[A](value: A)

    trait Printable[A] { self =>

      def format(value: A): String

      def contramap[B](func: B => A): Printable[B] =
        new Printable[B] {
          def format(value: B): String = {
            self.format(func(value))
          }
        }
    }


    implicit val stringPrintable: Printable[String] =
      new Printable[String]{
        override def format(value: String): String =  s"'${value}'"
      }

    implicit val booleanPrintable: Printable[Boolean] =
      new Printable[Boolean] {
        override def format(value: Boolean): String =
          if(value) "yes" else "no"
      }

    implicit def boxPrintable[A](implicit p:Printable[A]): Printable[Box[A]] = {
      p.contramap[Box[A]](_.value)
    }

    def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)


    assert(format("hello") == "'hello'")
    assert(format(true) == "yes")

    format(Box("hello world") == "'hello world'")
    format(Box(true) == "yes")

  }

  test("Codec examples") {

    trait Codec[A] { self =>

      def encode(value: A): String

      def decode(value: String): A

      def imap[B](dec: A => B, enc: B =>A): Codec[B] = {
        new Codec[B] {
          override def encode(value: B): String = self.encode(enc(value))

          override def decode(value: String): B = dec(self.decode(value))
        }
      }

    }

    def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

    def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

    implicit val stringCodec: Codec[String] = {
      new Codec[String] {
        override def encode(value: String): String = value

        override def decode(value: String): String = value
      }
    }

    implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt,_.toString)
    implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean,_.toString)
    implicit val doubleDodec: Codec[Double] = stringCodec.imap(_.toDouble,_.toString)

    final case class Box[A](value : A)

    implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = c.imap[Box[A]](Box(_),_.value)

  }


  test("contravariant in Cats") {

    val showString = Show[String]

    val showSymbol = Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}'")

    assert(showSymbol.show(Symbol("dave")) == "'dave'")

    val showSymbol2 = showString
      .contramap[Symbol](sym => s"'${sym.name}'")

    assert(showSymbol2.show(Symbol("dave"))=="'dave'")


  }

  test("invariants in Cats"){

    //implicit val symbolMonoid: Monoid[Symbol] =
      //Monoid[String].imap(Symbol.apply)(_.name)


    //Monoid[Symbol].empty

    //Symbol("a") |+| Symbol("few") |+| Symbol("words")


  }

  test("partial unification") {

    val func1 = (x: Int) => x.toDouble
    val func2 = (y: Double) => y * y

    val func3 = func1.map(func2)

    val either: Either[String,Int] = Right(123)

    assert(either.map(_ + 1) ==  Right(124))

    val func3a: Int => Double = a => func2(func1(a))

    val func3b: Int => Double = func2.compose(func1)





    // Left n page 76

  }




}
