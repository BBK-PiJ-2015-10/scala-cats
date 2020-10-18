package chapter4

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import cats.{Id, Monad}
import cats.instances.option._
import cats.instances.list._
import cats.instances.vector._
import cats.implicits._
import cats.instances.either._
import cats.MonadError

import scala.util.{Failure, Try}

//import cats.syntax.applicative._
//import cats.syntax.applicativeError._
//import cats.syntax.monadError._

import scala.util.Success
import cats.Eval


class MonadsExamplesTest extends AnyFunSuiteLike{

  test("Options monads"){

    def parseInt(str: String): Option[Int] = scala.util.Try(str.toInt).toOption

    def divide(a: Int, b: Int): Option[Int] = if (b == 0) None else Some(a/b)

    def divideStringBy(aString: String, bString: String) : Option[Int] =
      parseInt(aString).flatMap { aNum =>
        parseInt(bString).flatMap { bNum =>
          divide(aNum,bNum)
        }
      }

    def stringDividedBy(aString: String, bString: String) : Option[Int] =
      for {
        aNum <- parseInt(aString)
        bNum <- parseInt(bString)
        ans <- divide(aNum,bNum)
      } yield ans


    assert(divideStringBy("6","2") == Some(3))
    assert(divideStringBy("6","0") == None)
    assert(divideStringBy("6","food") == None)

    assert(stringDividedBy("6","2") == Some(3))
    assert(stringDividedBy("6","0") == None)
    assert(stringDividedBy("6","food") == None)


  }

  test("list monads"){

    val answer : List[(Int,Int)] = for {
      x <- (1 to 3).toList
      y <- (4 to 5).toList
    } yield (x,y)

    val expected = List((1,4),(1,5),(2,4),(2,5),(3,4),(3,5))

    assert(answer == expected)

  }

  test("Futures monads"){

    def doSomethingLongRunning: Future[Int] = ???
    def doSomethingElseLongRunning: Future[Int] = ???

    def doSomthingVeryLongRunning: Future[Int] =
      for {
        result1 <- doSomethingLongRunning
        result2 <- doSomethingElseLongRunning
      } yield result1 + result2

    def doSomethingVeryLongRunning: Future[Int] =
      doSomethingLongRunning.flatMap { result1 =>
        doSomethingElseLongRunning.map { result2 =>
          result1 + result2
        }
      }

  }


  test("Getting funcy") {

    trait MyMonad[F[_]] {

      def pure[A](a: A): F[A]

      def flatMap[A,B](value: F[A])(func: A => F[B]): F[B]

      def map[A,B](value: F[A])(func: A => B) : F[B] =
        flatMap(value)(a => pure(func(a)))

    }

  }

  test("cats monads"){

    val opt = Monad[Option]
    val opt1 = opt.pure(3)
    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a +2))
    val opt3 = Monad[Option].map(opt2)(a => 100 * a)

    assert(opt1 == Some(3))
    assert(opt2 == Some(5))
    assert(opt3 == Some(500))

    val list1 = Monad[List].pure(3)

    assert(list1 == List(3))

    val list2 = Monad[List].flatMap(List(1,2,3))(a => List(a, a*10))

    assert(list2 == List(1,10,2,20,3,30))

    val list3 = Monad[List].map(list2)(a => a + 123)
    assert(list3 == List(124,133,125,143,126,153))

  }

  test("default monads") {

    val res1 = Monad[Option].flatMap(Option(1))(a => Option(a*2))

    assert(res1 == Option(2))

    val res2 = Monad[Vector].flatMap(Vector(1,2,3))(a => Vector(a , a*10))

    //assert(res2 == Vector(1,10,2,20,3,30))

    val fm = Monad[Future]
    val future = fm.flatMap(fm.pure(1))(x => fm.pure(x+2))

    Await.result(future,1.second)

    assert(future.value.get.get == 3)

  }

  test("Monad syntax") {

    val res1 = 1.pure[Option]
    val res2 = 1.pure[List]

    assert(res1 == Option(1))
    assert(res2 == List(1))

    def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = a.flatMap(x => b.map(y => x*x + y*y))

    val res3 = sumSquare(Option(3),Option(4))
    val res4 = sumSquare(List(1,2,3),List(4,5))

    assert(res3 == Option(25))
    assert(res4 == List(17,26,20,29,25,34))

    def sumSquare2[F[_]: Monad](a: F[Int],b: F[Int]): F[Int] =
      for {
        x <- a
        y <- b
      } yield x*x + y*y

    val res5 = sumSquare(3: Id[Int], 4 : Id[Int])

    assert(res5 == 25)

   // page 84


  }

  test(" ids ") {


    val a = Monad[Id].pure(3)

    assert(a == 3)

    val b = Monad[Id].flatMap(a)(_ + 1)

    assert(b == 4)

    //left on page 89

  }

  test("Either") {

    val a = 3.asRight[String]
    val b = 4.asRight[String]

    for {
      x <- a
      y <- b
    } yield x*x + y*y

    def countPositive(nums: List[Int]) =
      nums.foldLeft(0.asRight[String]){(acc,num) =>
        if (num > 0 ){
          acc.map(_ + 1)
        } else {
          Left("Negative. Stopping")
        }
      }

    assert(countPositive(List(1,2,3,45)) == Right(4))

    assert(countPositive(List(1,2,-3,45)) == Left("Negative. Stopping"))


    val error : Either[NumberFormatException,Int] = Either.catchOnly[NumberFormatException]("foo".toInt)

    //assert(error == Left(new NumberFormatException("For input string: \"foo\"")))

    val nonFatal : Either[Throwable,Nothing] = Either.catchNonFatal(sys.error("Badness"))

    //assert(nonFatal == Left("Badness"))


    val fromTry : Either[Throwable,Int] = Either.fromTry(scala.util.Try("foo".toInt))

    //assert(fromTry == Left)

    val fromOption : Either[String,Int] = Either.fromOption[String,Int](None,"Badness")

    assert(fromOption == Left("Badness"))


    //assert(error == NumberFormatException)


  }

  test ("Transforming Eithers") {


    val t1 = "Error".asLeft[Int].getOrElse(0)
    assert (t1 == 0)

    val t2 = "Error".asLeft[Int].orElse(2.asRight[String])
    assert (t2 == Right(2))

    val t3 = -1.asRight[String].ensure("Must be non negative")(_ > 0)
    assert (t3 == Left("Must be non negative"))

  }

  test ("recover and recoverWith") {

    val t1 = "error".asLeft[Int].recover {
      case _: String => -1
    }

    assert(t1 == Right(-1))

    val t2 = "error".asLeft[Int].recoverWith {
      case _: String => Right(-1)
    }

    val t3= "foo".asLeft[Int].leftMap(_.reverse)
    assert (t3 == Left("oof"))

    val t4 = 6.asRight[String].bimap(_.reverse,_ *7)
    assert (t4 == Right(42))

    val t5 = "bar".asLeft[Int].bimap(_.reverse,_ * 7)
    assert (t5 == Left("rab"))

    val t6 = 123.asRight[String]
    val t7 = t6.swap

    assert(t6 == Right(123))
    assert(t7 == Left(123))

  }


  test("Error Handling") {

    val t1 = for {
      a <- 1.asRight[String]
      b <- 0.asRight[String]
      c <- if (b == 0) "DIV0".asLeft[String] else (a/b).asRight[String]
    } yield c

    assert(t1 == Left("DIV0"))

    type Result[A] = Either[Throwable,A]

    object wrapper {

      sealed trait LoginError extends Product with Serializable

      final case class UserNotFound(username: String) extends LoginError

      final case class PasswordIncorrect(username: String) extends LoginError

      case object UnexpectedError extends LoginError

    }
    import wrapper._

    case class User(username: String, password: String)

    type LoginResult = Either[LoginError,User]

    def handleError(error: LoginError): Unit =
      error match {
        case UserNotFound(u) => println(s"User not found: $u")
        case PasswordIncorrect(u) => println(s"Password incorrect: $u")
        case UnexpectedError => println(s"Unexpected error")
      }

    val result1 : LoginResult = User("Dave","password").asRight
    val result2 : LoginResult = UserNotFound("dave").asLeft

    assert(result1 == Right(User("Dave","password")))
    assert(result2 == Left(UserNotFound("dave")))

    result1.fold(handleError,println)
    result2.fold(handleError,println)


  }

  test("Monad error") {
    import cats.MonadError
    import cats.instances.either._
    import cats.syntax.applicative._ // for pure
    import cats.syntax.applicativeError._ // for raiseError etc
    import cats.syntax.monadError._

    type ErrorOr[A] = Either[String, A]
    val monadError = MonadError[ErrorOr, String]

    val success : ErrorOr[Int] = monadError.pure(42)
    assert (success == Right(42))

    val failure : ErrorOr[String]  = monadError.raiseError("Badness")
    assert (failure == Left("Badness"))

    val f2 = monadError.handleErrorWith(failure){
      case "Badness" => monadError.pure("It's ok")
      case _ => monadError.raiseError("It's not ok")
    }
    assert(f2 == Right("It's ok"))

    val t1 = monadError.ensure(success)("Number too low")(_ > 1000)
    assert(t1 == Left("Number too low"))

    val success2 = 42.pure[ErrorOr]
    assert(success == Right(42))

    val failure2 : ErrorOr[Int] = "Badness".raiseError[ErrorOr, Int]
    assert(failure2 == Left("Badness"))

    //failure2.handleErrorWith {
      //case "Badness" => 256.pure
      //case _ => ("It is not ok").raiseError
    //}

    val res: ErrorOr[Int] = success2.ensure("Number too low!")(_ > 1000)
    assert( res == Left("Number too low!"))


  }

  test("instances of Monad error") {

    val exn: Throwable = new RuntimeException("Don't mess with me")

    val result  = exn.raiseError[Try,Int]

  }

  test("validate adult example"){

    //type ExceptionOr[A] = Either[Throwable,A]

    def validateAdult[F[_]](age: Int)(implicit me: MonadError[F,Throwable]) : F[Int] = {
      if (age >= 18) age.pure[F]
      else new IllegalArgumentException("Age must be greater than or equal to 18").raiseError[F,Int]
    }

    val t1 = validateAdult[Try](18)
    assert(t1 == Success(18))

    val t2 = validateAdult[Try](17)
    //assert(t2 == Failure(new IllegalArgumentException("Age must be greater than or equal to 18")))

  }

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
