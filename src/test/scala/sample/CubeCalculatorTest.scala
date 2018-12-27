package sample

import org.scalatest.FunSuite

class CubeCalculatorTest  extends FunSuite{
  test("SampleCubeCalculator.cube"){
    assert(SampleCubeCalculator.cube(3) === 27)
  }
}
