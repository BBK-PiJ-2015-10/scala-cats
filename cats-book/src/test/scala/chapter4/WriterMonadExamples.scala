package chapter4

import cats.Id
import org.scalatest.funsuite.AnyFunSuiteLike
import cats.data.{Writer, WriterT}
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._


class WriterMonadExamples extends AnyFunSuiteLike {

  test("Writer monads"){

    val w1 = Writer(Vector(
      "It was the best of times",
      "It was the worst of times"
    ),1859)

    //type Writer[W,A] = WriterT[Id,W,A]

    type Logged[A] = Writer[Vector[String],A]

    //val r1 = 123.pure[Logged]

    //assert(r1 == WriterT((Vector(),123)))

   val r2 = Vector("msg1","msg2","msg3").tell

    val a = Writer(Vector("msg1","msg2","msg3"),123)

    val b = 123.writer(Vector("msg1","msg2","msg3"))

    assert ( a == b)

    val aResult = a.value
    val aLog = a.written

    assert(aResult == 123)
    assert(aLog == Vector("msg1","msg2","msg3") )

    val (log,result) = b.run
    assert(result == 123)
    assert(log == Vector("msg1","msg2","msg3") )

  }

  test("Composing and transforming writers") {


    type Logged[A] = Writer[Vector[String],A]

    val writer1 = for {
      a <- 10.pure[Logged]
      _ <- Vector("a","b","c").tell
      b <- 32.writer(Vector("x","y","z"))
    } yield a + b

    val r1= writer1.run
    assert (r1._1 == Vector("a","b","c","x","y","z"))
    assert (r1._2 == 42)

    val writer2 = writer1.mapWritten(_.map(_.toUpperCase()))
    val r2 = writer2.run
    assert (r2._1 == Vector("A","B","C","X","Y","Z"))

    val writer3 = writer1.bimap(
      log => log.map(_.toUpperCase()),
      res => res * 100
    )

    val r3 = writer3.run
    assert (r3._1 == Vector("A","B","C","X","Y","Z"))
    assert (r3._2 == 4200)

    val writer4 = writer3.mapBoth{ (log,res) =>
      val log2 = log.map(_ + "!")
      val res2 = res * 100
      (log2,res2)
    }

    val writer5 = writer1.reset
    val r5 = writer5.run
    assert (r5._1 == Vector())
    assert (r5._2 == 42)

    val writer6 = writer1.swap
    val r6 = writer6.run
    assert (r6._1 == 42 )
    assert (r6._2 == Vector("a","b","c","x","y","z"))

  }

  test("example of writer") {

    def slowly[A](body: =>A) = try body finally Thread.sleep(100)

    def factorial(n: Int) : Int = {
      val ans = slowly(if(n == 0) 1 else n * factorial(n-1))
      println(s"fact $n $ans")
      ans
    }

    //factorial(5)

    //Await.result(Future.sequence(Vector(
      //Future(factorial(5)),
      //Future(factorial(5))
    //)), 5.seconds)

    type Logged[A] = Writer[Vector[String],A]

    def fac(n: Int) : Logged[Int] = {
      for {
        ans <- if (n == 0){1.pure[Logged]} else {slowly(fac(n-1).map(_ * n))}
        _   <- Vector(s"fact $n $ans").tell
      } yield ans
    }

    Await.result(Future.sequence(Vector(
    Future(fac(5)),
    Future(fac(5))
    )), 5.seconds)

    //Left on page 115







  }

}
