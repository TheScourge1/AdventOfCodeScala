package advent2017

import org.scalatest.FunSuite

class Test6 extends FunSuite {

  val ex = Exercise6

  test("Exercise6.ex1") {
    assert(ex.ex1("0 2 7 0") === "5")

    println("Solution 1: "+ex.ex1("4\t1\t15\t12\t0\t9\t9\t5\t5\t8\t7\t3\t14\t5\t12\t3"))
  }

  test("Exercise6.ex2") {
    assert(ex.ex2("0 2 7 0") === "4")

    println("Solution 2: "+ex.ex2("4\t1\t15\t12\t0\t9\t9\t5\t5\t8\t7\t3\t14\t5\t12\t3"))
  }
}