package advent2017

import org.scalatest.FunSuite

class Test3_2 extends FunSuite {

  val ex = Exercise3_2

  test("Exercise3_2.ex1") {
    assert(ex.ex1("1") === "0")
    assert(ex.ex1("12") === "3")
    assert(ex.ex1("23") === "2")
    assert(ex.ex1("1024") === "31")

    println("Solution 1: "+ex.ex1("325489"))
  }

  test("Exercise3_2.ex2") {
    assert(ex.ex2("1") === "2")
    assert(ex.ex2("23") === "25")
    assert(ex.ex2("59") === "122")
    assert(ex.ex2("747") === "806")

    println("Solution 2: "+ex.ex2("325489"))
  }
}