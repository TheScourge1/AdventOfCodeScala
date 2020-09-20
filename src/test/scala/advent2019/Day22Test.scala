package advent2019

import advent2019.Day22.{Mcalc, getActionList, getLinearFunction, getReverseActionList, getSourceFromTarget}
import org.scalatest.FunSuite

import scala.io.Source

class Day22Test extends FunSuite{

  val cardCount = 119315717514047L
  val input = readFile("Day22.txt").toList

  private def readFile(fname: String): Seq[String] = {
    val fileHandle = getClass.getResourceAsStream("/2019/"+fname)
    if(fileHandle == null) throw new Exception("File not found: "+"/2019/"+fname)
    Source.fromInputStream(fileHandle).getLines.toSeq
  }

  test("Day22.McalcPow"){
    val a = Day22.Mcalc(2,9)
    assert(a.pow(2).v == 4)
    assert(a.pow(3).v == 8)
    assert(a.pow(4).v == 7)
  }

  test("Day22.testShuffleParams"){
    val n4 = 2020
    val n3 = getSourceFromTarget(n4,input,cardCount)
    val n2 = getSourceFromTarget(n3,input,cardCount)
    val n1 = getSourceFromTarget(n2,input,cardCount)

    val fPars = getLinearFunction(input,cardCount)

    var testVal = n3
    for(action <- getActionList(input)) testVal = action._2(testVal,action._1,cardCount)
    assert(testVal == n4,"n3 -> n4")

    testVal = Mcalc(n2,cardCount).mul(Mcalc(fPars._1,cardCount)).add(Mcalc(fPars._2,cardCount)).v
    assert(testVal == n3,"n2 -> n3")

    testVal = Mcalc(n1,cardCount).mul(Mcalc(fPars._1,cardCount)).add(Mcalc(fPars._2,cardCount)).v
    assert(testVal == n2,"n1 -> n2")
  }

  test("Day22.itterations"){

  }

}
