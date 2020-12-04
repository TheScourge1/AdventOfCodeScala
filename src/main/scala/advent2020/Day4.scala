package advent2020

object Day4 extends Day {
  override def day() = 4

  override def testSetA = List(TestCase("Day4test.txt","2"))

  override def testSetB = List(TestCase("",""))

  override def solutionA(input: List[String], params: List[String]) = {
    countValidPasses(input,isValidPass).toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    countValidPasses(input,isValidPass2).toString
  }

  def countValidPasses(input: List[String],passValidator:Map[String,String] => Boolean):Int ={
    var passwords = List[Map[String,String]]()
    val emptyIndexes = List(0) ++ input.zipWithIndex.filter(_._1 == "").map(_._2) ++ List(input.size)
    for(i <- 0 until emptyIndexes.size-1)
      passwords = passwords :+ readpassword(input.slice(emptyIndexes(i),emptyIndexes(i+1)).filter(_ != ""))

    passwords.count(p => passValidator(p))
  }

  def readpassword(passData: List[String]): Map[String,String] = {
    passData.map(
      p => p.split(" ")
        .map(ss => ss.split(":"))
        .map(sss => sss(0) -> sss(1))
        .toMap
    ).fold(Map[String,String]())( _ ++ _)
  }

  def isValidPass(passes: Map[String,String]): Boolean = {
    if((passes.keySet -- Set("byr","iyr","eyr","hgt","hcl","ecl","pid","cid")).size > 0) false
    else (passes.keySet - "cid").size >=7
  }

  def isValidPass2(passes: Map[String,String]): Boolean = {
    val keys = passes.keys.toSet - "cid"
    def checkBetween(s:String, lower:Int,upper:Int) =
      s.matches("[0-9]+") && s.toInt >= lower && s.toInt <= upper

    for (k <- keys) {
      val errVal = k match {
        case "byr" => if (checkBetween(passes(k),1920,2002)) "" else "byr"
        case "iyr" => if (checkBetween(passes(k),2010,2020)) "" else "iyr"
        case "eyr" => if (checkBetween(passes(k),2020,2030))  "" else "eyr"
        case "hgt" => passes(k).takeRight(2) match {
          case "cm" => if(checkBetween(passes(k).take(passes(k).size - 2),150,193) ) "" else "hgt"
          case "in" => if (checkBetween(passes(k).take(passes(k).size - 2),59,76) ) "" else "hgt"
          case _ => "hgt"
        }
        case "hcl" => if (passes(k).matches("#([0-9a-f]){6}")) "" else "hcl"
        case "ecl" => if (Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(passes(k))) "" else "ecl"
        case "pid" => if (passes(k).size == 9  && passes(k).matches("[0-9]+")) "" else "pid"
        case _ => k
      }
      if (errVal != "") return false
    }
    keys.size >= 7
  }
}
