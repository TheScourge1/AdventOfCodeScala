package advent2020

object Day19 extends Day {
  override def day() = 19

  override def testSetA = List(TestCase("0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\nababbb\nbababa\nabbbab\naaabbb\naaaabbb","2"))

  override def testSetB = List(TestCase("Day19test.txt","12"))

  override def solutionA(input: List[String], params: List[String]) = {
    val data = readRulesAndCodes(input)
    val rules = data._1
    val codes = data._2

    val ruleSizes: Map[Rule,Int] = rules.map(r => r -> calcRuleSize(r,rules)).toMap
    codes.count(p => matchRule(p,rules.find(_.name == "0").get,ruleSizes)).toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    var input2 = input.filter(p => !(p.startsWith("8:") || p.startsWith("11:")))
    input2 = input2 :+ "8: 42 | 42 8" :+ "11: 42 31 | 42 11 31"
    val data = readRulesAndCodes(input2)
    val rules = data._1
    val codes = data._2

    val validCodes = for(code <- codes if(isValidBCode(code,rules))) yield code
    validCodes.size.toString
  }

  def isValidBCode(code:String,rules:Set[Rule]): Boolean = {
    val codes42 = generateCodeOptions("42",rules)
    val codes31 = generateCodeOptions("31",rules)

    val endOptions = checkCodeEndOptions(code,codes31)
    for(endOption <- endOptions){
      val endCode  = endOption.fold("")(_+_)
      var startOptions = checkCodeEndOptions(code.dropRight(endCode.size),codes42)
      startOptions = startOptions.filter(o => o.size > endOption.size)
      startOptions = startOptions.filter(o => o.fold("")(_+_).size + endCode.size == code.size)
      if(startOptions.size > 0) return true
    }
    false
  }

  def checkCodeEndOptions(code:String,codeParts:List[String]): List[List[String]] = {
    var result = List[List[String]]()
    for(codePart <- codeParts){
      if(code.endsWith(codePart)){
        result = result :+ List(codePart)
        val nextOptions = checkCodeEndOptions(code.dropRight(codePart.size),codeParts)
        for(option <- nextOptions){
          result = result :+ (List(codePart) ++ option)
        }
      }
    }
    result
  }

  def generateCodeOptions(startCode: String,rules: Set[Rule]): List[String] = {
    val rule = rules.filter(_.name == startCode).head
    if(rule.isLiteral) List(rule.options.head.head)
    else{
      var result = List[String]()
      for(option <- rule.options){
        val firstOptions = generateCodeOptions(option.head,rules)
        val secondOptions = if(option.size > 1) generateCodeOptions(option.last,rules) else List("")
        for(firstOption <- firstOptions;secondOption<- secondOptions) result = result :+ (firstOption + secondOption)
      }
      result
    }
  }

  def readRulesAndCodes(input: List[String]): (Set[Rule],List[String])= {
    var rules = Set[Rule]()
    var codes = List[String]()

    for(s <- input.filter(_ !="")) {
      if (s.contains(":")){
        val ruleName = s.slice(0, s.indexOf(":"))
        val optionStrings =  s.drop(s.indexOf(":") + 1).split("[|]").map(s => s.drop(1).replaceAll("\"", "")).toSet
        rules = rules + Rule(ruleName,optionStrings.map(o => o.split(" ").toList))
      }
      else codes = codes :+ s.drop(s.indexOf(":") + 1).replaceAll(" ", "")
    }
    (rules,codes)
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

  def calcRuleSize(rule: Rule, rules: Set[Rule]): Int ={
    if(rule.isLiteral) 1
    else {
      val ruleSizes = (for(option <- rule.options) yield
        option.map(s => rules.find(_.name == s).get).map(subRule => calcRuleSize(subRule,rules)).sum).toSet
      if(ruleSizes.size > 1) throw new Exception("Multiple size options found")
      else ruleSizes.head
    }
  }

  case class Rule(name:String,options: Set[List[String]]){
    val isLiteral = options.size == 1 && options.head.size == 1 &&
      options.head.count(_.matches("[a-z]+")) == 1
  }
}
