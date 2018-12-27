package advent2018

import org.scalatest.FunSuite

class Test1 extends FunSuite {

  private val ex = Exercise1

  test("Exercise1.ex1") {
    val input = Vector("+1","+1","+1")
    assert(ex.ex1(input) === "3")

    println("Solution 1: "+ex.ex1FromFile("2018/ex1_1.txt"))
  }

  test("Exercise1.ex2") {
    val input = Vector("+3","+3","+4","-2","-4")
    assert(ex.ex2(input) === "10")

    println("Solution 2: "+ex.ex2FromFile("2018/ex1_1.txt"))
  }
}