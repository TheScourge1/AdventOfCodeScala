package advent2018

import scala.io.Source

trait AdventBase {

  def ex1(input: Seq[String]): String
  def ex2(input: Seq[String]): String

  def ex1(input: String): String = ex1(Seq(input))
  def ex2(input: String): String = ex2(Seq(input))

  def ex1FromFile(fname: String): String = ex1(readFile(fname))
  def ex2FromFile(fname: String): String = ex2(readFile(fname))


  def readFile(fname: String): Seq[String] = {
    val fileHandle = getClass.getResourceAsStream("/"+fname)
    return Source.fromInputStream(fileHandle).getLines.toSeq
  }
}
