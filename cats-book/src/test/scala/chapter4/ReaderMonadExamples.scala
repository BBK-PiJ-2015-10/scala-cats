package chapter4

import org.scalatest.funsuite.AnyFunSuiteLike
import cats.data.Reader
import cats.implicits.catsSyntaxApplicativeId

class ReaderMonadExamples extends AnyFunSuiteLike{

  final case class Cat(name: String, favoriteFood: String)


  test("Creating and unpacking Readers"){


    val catName: Reader[Cat,String] = Reader(cat => cat.name)

    val r1 = catName.run(Cat("Garfield","lasagne"))

    assert(r1 == "Garfield" )

  }

  test ("composing readers"){

    val greetKitty : Reader[Cat,String] = Reader(cat => s"Hello ${cat.name}")

    val feedKitty : Reader[Cat,String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

    val greetAndFeed : Reader[Cat,String] =
      for {
        greet <- greetKitty
        feed  <- feedKitty
      } yield s"$greet. $feed."

    val r1 = greetAndFeed(Cat("Garfield","Pizza"))

    assert (r1 == "Hello Garfield. Have a nice bowl of Pizza.")

  }

  test("Login example") {

    final case class DB(usernames: Map[Int,String], passwords: Map[String,String])

    type DBReader[A] = Reader[DB,A]

    def findUsername(userId: Int) : DBReader[Option[String]] =  Reader(db => db.usernames.get(userId))

    def checkpassword(username: String, password: String): DBReader[Boolean] =
      Reader(db => db.passwords.get(username).contains(password))

    def checkLogin(userId: Int, password: String): DBReader[Boolean] =
      for {
        username <- findUsername(userId)
        passwordOk <- username.map { username =>
            checkpassword(username,password)
        }.getOrElse{
          false.pure[DBReader]
        }
      } yield passwordOk

    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )

    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )
    val db = DB(users, passwords)
    val r1 = checkLogin(1, "zerocool").run(db)
    // res7: cats.package.Id[Boolean] = true
    val r2 = checkLogin(4, "davinci").run(db)

    assert(r1 == true)
    assert(r2 == false)

  }

  //Left on page 118

}
