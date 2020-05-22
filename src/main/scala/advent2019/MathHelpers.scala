package advent2019

object MathHelpers {

  def getFactors(in: Int): List[Int] = {
    var rem = in
    var factor = 2
    var result = List[Int]()
    while(rem/factor > 2){
      if(rem%factor == 0) {
        result = result :+ factor
        rem = rem/factor
      }
      else factor += 1
    }

    (result :+ rem)
  }

}
