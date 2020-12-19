package advent2020

object Day19 extends Day {
  override def day() = 19

  override def testSetA = List(TestCase("0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\nababbb\nbababa\nabbbab\naaabbb\naaaabbb","2"))

  override def testSetB = List(TestCase("",""))

  override def solutionA(input: List[String], params: List[String]) = {
    var rules = List[Rule]()
    var codes = List[String]()

    for(s <- input.filter(_ !="")) {
      if (s.contains(":")){
        val ruleName = s.slice(0, s.indexOf(":"))
        val optionStrings =  s.drop(s.indexOf(":") + 1).split("[|]").map(s => s.drop(1).replaceAll("\"", "")).toSet
        rules = rules :+ Rule(ruleName,optionStrings.map(o => o.split(" ").toList))
      }
      else codes = codes :+ s.drop(s.indexOf(":") + 1).replaceAll(" ", "")
    }

    val ruleSizes: Map[Rule,Int] = rules.map(r => r -> calcRuleSize(r,rules)).toMap
    codes.count(p => matchRule(p,rules.find(_.name == "0").get,ruleSizes)).toString
  }

  override def solutionB(input: List[String], params: List[String]) = {

    "TODO"
  }


  def matchRule(code: String, rule: Rule,rules: Map[Rule,Int]): Boolean ={
    if(rule.isLiteral)
      if(code.size == 1) return code == rule.options.head.head
      else return false
    else{
      for(option <- rule.options){
        var index = 0
        val optionRules = option.map(o => rules.keySet.find(_.name == o).get)
        var optionValid = true
        for(subRule <- optionRules){
          if(!matchRule(code.slice(index,index+rules(subRule)),subRule,rules)) optionValid = false
          index += rules(subRule)
        }
        if(optionValid) return index == code.size // found a valid option, just check if all code got used in matching
      }
      false //none of the options is valid
    }
  }

  def calcRuleSize(rule: Rule, rules: List[Rule]): Int ={
    if(rule.isLiteral) 1
    else {
      val ruleSizes = (for(option <- rule.options) yield
        option.map(s => rules.find(_.name == s).get).map(subRule => calcRuleSize(subRule,rules)).sum).toSet
      if(ruleSizes.size > 1) throw new Exception("Multiple size options found")
      else ruleSizes.head
    }
  }

  case class Rule(name:String,options: Set[List[String]]){
    val isLiteral = options.size == 1 && options.head.count(_.matches("[a-z]+")) == 1
  }
}
