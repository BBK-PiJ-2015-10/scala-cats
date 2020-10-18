package chapter4

import cats.implicits._
import cats.{Id, Monad, MonadError}
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, _}
import scala.concurrent.duration._
import scala.util.Try

//import cats.syntax.applicative._
//import cats.syntax.applicativeError._
//import cats.syntax.monadError._

import cats.Eval

import scala.util.Success


class EvalMonadsExamples extends AnyFunSuiteLike{

  test("eval monads"){

    val x = {
      println("Computing X")
      math.random()
    }

    val r1 = x
    val r2 = x

    assert(r1 == r2)

    def y = {
      println("Computing Y")
      math.random()
    }

    val r3 = y
    val r4 = y

    assert(r3 != r4)

    lazy val z = {
      println("Computing Z")
      math.random()
    }

  }

  test("Now, Always, Later") {

    val now = Eval.now(math.random() + 1000)

    val always = Eval.always(math.random() + 3000)

    val later = Eval.later(math.random() + 2000)

    now.value

    always.value

    later.value

    val x = Eval.now {
      println("Computing X")
      math.random()
    }

    x.value
    x.value

    val y = Eval.always{
      println("Computing Y")
      math.random()
    }

    y.value
    y.value

    val z = Eval.later{
      println("Computing Z")
      math.random()
    }

    z.value
    z.value

  }

  test("Eval as a Monad") {

  val greeting = Eval.always{
    println("Step 1")
    "Hello"
  }.map{ str =>
    println("Step 2")
    s"$str world"
  }

  val r1 = greeting.value
  assert (r1 == "Hello world")


  val ans = for {
    a <- Eval.now{ println("Calculating A"); 40}
    b <- Eval.always{ println("Calculating B") ; 2}
  } yield {
    println("Adding A and B")
    a + b
  }

  assert(ans.value ==  42)
    assert(ans.value ==  42)

  val saying = Eval
    .always{println("Step 1"); "The cat"}
    .map{str => println("Step 2"); s"$str sat on"}
    .memoize
    .map{str => println("Step 3"); s"$str the mat"}

  saying.value
  saying.value

  }

  test("Trampolining and Eval.defer") {

    def factorial(n: BigInt): BigInt  =
      if (n == 1 ) n else n * factorial(n -1)

    def factorial2(n: BigInt): BigInt = helperFactorial(1)(n-1)

    def helperFactorial(acc: BigInt)(n: BigInt) : BigInt = {
      if (n == 1) acc * n
      else helperFactorial(acc * n)(n-1)
    }

    def factorialEval(n: BigInt): Eval[BigInt] =
      if (n == 1) {
        Eval.now(n)
      } else {
        Eval.defer(factorialEval(n -1).map(_ * n))
      }

    val r1 = factorialEval(50000).value


  }

  test("folding eval") {

    def naiveFoldRight[A,B](as: List[A], acc: B)(fn: (A,B) => B): B =
      as match {
        case head :: tail =>
          fn(head,(naiveFoldRight(tail,acc)(fn)))
        case Nil => acc
      }

    def foldRightEval[A,B](as: List[A], acc: Eval[B])(fn: (A,Eval[B]) => Eval[B]): Eval[B] =
      as match {
        case Nil => acc
        case head :: tail => Eval.defer(fn(head,foldRightEval(tail,acc)(fn)))
      }

    def nonNaiveFoldRight[A,B](as: List[A], acc: B)(fn: (A,B) => B): B =
      foldRightEval(as,Eval.now(acc)){ (a,b) =>
        b.map(fn(a,_))
      }.value

  }


}
