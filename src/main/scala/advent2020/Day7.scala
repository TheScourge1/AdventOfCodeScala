package advent2020

object Day7 extends Day {
  override def day() = 7

  override def testSetA = List(TestCase("light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.", "4"))

  override def testSetB = List(TestCase("light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.", "32"),
    TestCase("shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags.", "126"))

  override def solutionA(input: List[String], params: List[String]): String = {
    val bags = input.map(s => readBag(s))
    bagsCanContain(bags,"shiny gold").size.toString
  }

  override def solutionB(input: List[String], params: List[String]): String = {
    val bags = input.map(s => readBag(s))
    (getBagCost(bags,"shiny gold")-1).toString
  }

  def readBag(input: String): Bag = {
    val bagParse = "([0-9]+) ([a-z ]+) bag".r
    val bagName = input.split(" bags contain" ).head
    val subBags = (for(b <- bagParse.findAllMatchIn(input)) yield b.group(2) -> b.group(1).toInt).toMap
    Bag(bagName,subBags)
  }

  def bagsCanContain(bags: List[Bag],bagName: String):Set[String] = {
    val parentBags = bags.filter(b => b.containingBags.keySet.contains(bagName)).map(_.name).toSet
    val subBags =  (for(b <- parentBags) yield bagsCanContain(bags,b)).flatten
    parentBags ++ subBags
  }

  def getBagCost(bags: List[Bag],bagName: String):Long = {
    val subBags = bags.filter(_.name == bagName).head.containingBags
    1+(for(b <- subBags.keySet.toList) yield subBags(b) * getBagCost(bags,b)).sum
  }

  case class Bag(name: String,containingBags: Map[String,Int])
}
