package advent2019

object Day14 extends Day(14){
  override def testSetA = List(TestCase("9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL","165"),
    TestCase("157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT","13312"))
  override def testSetB = List(TestCase("157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT","82892753"))

  override def solutionA(input: List[String], params: List[String]) = {
    val formulas = getFormulas(input)
    requiredAmount(formulas,"ORE",1).toString
  }




  override def solutionB(input: List[String], params: List[String]) = {
    var formulas = getFormulas(input)
    val ORESTOCK = 1000000000000L
    var lowerBound = ORESTOCK/requiredAmount(formulas,"ORE",1)
    var upperBound = ORESTOCK
    while (upperBound-lowerBound > 1){
      val attempt =  requiredAmount(formulas,"ORE",(lowerBound+upperBound)/2)
      if(attempt > ORESTOCK) upperBound = (lowerBound+upperBound)/2
      else lowerBound = (lowerBound+upperBound)/2
      //println(s"${lowerBound} - ${upperBound}")
    }
    lowerBound.toString
  }


  def getFormulas(input:List[String]): Map[String,(Chemical, List[Chemical])] = {
    var formulas = Map[String,(Chemical, List[Chemical])]()
    val pattern = "[0-9]+ [A-Z]+".r

    for(formula <- input){
      val chemList = pattern.findAllIn(formula)
        .toList
        .reverse
        .map(s => Chemical(s.split(" ")(1),s.split(" ")(0).toInt))
      formulas = formulas + (chemList.head.name -> (chemList.head,chemList.tail))
    }
    formulas
  }

  def requiredAmount(formulas: Map[String,(Chemical, List[Chemical])],resource: String,fuelQuantity:Long): Long = {
    var result = 0L
    if(resource=="FUEL") result = fuelQuantity
    else {
      val targets = formulas.filter(p => p._2._2.map(c =>c.name).contains(resource))
      for(target <- targets.keySet){
        val requiredQuantity = requiredAmount(formulas,target,fuelQuantity)
        val formulaExecutions = Math.ceil(requiredQuantity.toDouble/targets(target)._1.quantity).toLong
        val quantityPerFormula = targets(target)._2.filter(p => p.name == resource).head.quantity
        result += formulaExecutions * quantityPerFormula.toLong
      }
    }
    result
  }
  case class Chemical(name: String,quantity:Int) {
    override def toString = s"${quantity} ${name}"
  }

}
