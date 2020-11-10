package advent2017

import org.scalatest.FunSuite

class Test1 extends FunSuite {

  val ex = Exercise1

  test("Exercise1.ex1") {
    assert(ex.ex1("1122") === "3")
    assert(ex.ex1("1111") === "4")
    assert(ex.ex1("1234") === "0")
    assert(ex.ex1("91212129") === "9")

    println("Solution 1: "+ex.ex1FromFile("2017/ex1_1.txt"))
  }

  test("Exercise1.ex2") {
    assert(ex.ex2("1212") === "6")
    assert(ex.ex2("1221") === "0")
    assert(ex.ex2("123425") === "4")
    assert(ex.ex2("123123") === "12")

    println("Solution 2: "+ex.ex2FromFile("2017/ex1_1.txt"))
  }
}