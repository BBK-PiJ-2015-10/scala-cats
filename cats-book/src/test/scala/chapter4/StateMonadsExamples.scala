package chapter4

import org.scalatest.funsuite.AnyFunSuiteLike

import cats.data.State

class StateMonadsExamples extends AnyFunSuiteLike{

  test("Creating and unpacking state"){

    val a = State[Int,String]{ state =>
      (state, s"The state is $state")
    }

    val r1 = a.run(10).value

    assert(r1._1 == 10)
    assert(r1._2 == "The state is 10")

    val justState = a.runS(10).value

    assert(justState == 10)

    val justResult = a.runA(10).value

    assert(justResult == "The state is 10")

  }

  test("Composing and transforming state"){

    val step1 = State[Int,String]{ num =>
      val ans = num + 1
      (ans,s"Result of step1: $ans")
    }

    val step2 = State[Int,String]{ num =>
      val ans = num * 2
      (ans,s"Result of step2: $ans")
    }

    val both = for {
      a <- step1
      b <- step2
    } yield(a,b)


    val (state,result) = both.run(20).value
    assert(state == 42)
    assert(result == ("Result of step1: 21", "Result of step2: 42"))

  }


}
