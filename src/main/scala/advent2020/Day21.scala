package advent2020

object Day21 extends Day{
  override def day() = 21

  override def testSetA = List(TestCase("mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)","5"))

  override def testSetB = List(TestCase("mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)","mxmxvkd,sqjhc,fvjkl"))

  override def solutionA(input: List[String], params: List[String]) = {
    val foods = readFoods(input)
    val alergenInIngredient = findAlergenInIngredient(foods)
    foods.map(f => (f.ingredients -- (alergenInIngredient.values)).toList).flatten.size.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val foods = readFoods(input)
    val alergenInIngredient = findAlergenInIngredient(foods)

    val ingredients = for(alergen <- alergenInIngredient.keySet.toList.sorted) yield alergenInIngredient(alergen)
    ingredients.foldLeft("")((l,s) => l + s + ",").dropRight(1)
  }

  def findAlergenInIngredient(foods: List[Food]): Map[String,String] = {
    var alergenInFoods = Map[String,List[Int]]()
    for(food <- foods)
      for(alergen <- food.someAlergens)
        alergenInFoods += alergen -> (alergenInFoods.getOrElse(alergen,List[Int]()):+food.id)

    var alergenInIngredient = Map[String,String]()
    while(alergenInFoods.size > 0) {
      for (alergen <- alergenInFoods.keySet) {
        val foodsWithAlergene = foods.filter(f => alergenInFoods(alergen).contains(f.id))
        var ingredientsInAll =
          foodsWithAlergene.foldLeft(foods.map(_.ingredients).flatten.toSet)(
            (ingriedents, food) => ingriedents.intersect(food.ingredients))
        ingredientsInAll = ingredientsInAll  -- alergenInIngredient.values
        if (ingredientsInAll.size == 1) {
          alergenInIngredient += (alergen -> ingredientsInAll.head)
          alergenInFoods = alergenInFoods - alergen
        }
      }
    }
    alergenInIngredient
  }

  def readFoods(input:List[String]): List[Food] = {
    var result = List[Food]()
    var id = 1
    for(line<-input){
      val splitIndex = line.indexOf("(")
      val foods = line.slice(0,splitIndex).split(" ").toSet
      val ingredients = line.slice(splitIndex+"(contains ".size,line.size-1).split(", ").toSet
      result = result :+ Food(id,foods,ingredients)
      id +=1
    }
    result
  }
}

  case class Food(id:Int,ingredients:Set[String],someAlergens:Set[String])

