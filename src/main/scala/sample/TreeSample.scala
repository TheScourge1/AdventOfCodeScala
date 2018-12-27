package sample

object TreeSample {

  def main(args: Array[String]): Unit = {
    val exp: Tree = Sum(Sum(Var("x"),Var("x")),Sum(Const(7),Var("y")))
    val env: Environment = { case "x" => 5 case "y" => 7}

    println("Expression "+exp)
    println("Evaluation with x=5, y=7: "+eval(exp,env))
    println("Derivate relative to x:\n"+derive(exp,"x") + " = " + simplify(derive(exp,"x")))
    println("Derivate relative to y:\n"+derive(exp,"y") + " = " + simplify(derive(exp,"y")))
  }

  abstract class Tree
  case class Sum(l: Tree, r: Tree) extends Tree
  case class Var(n: String) extends Tree
  case class Const(v: Int) extends Tree

  def eval(t: Tree, env: Environment): Int = t match {
    case Sum(l,r) => eval(l,env) + eval(r,env)
    case Var(n) => env(n)
    case Const(v) => v
  }

  def derive(t: Tree, v: String): Tree = t match {
    case Sum(l,r) => Sum(derive(l,v), derive(l,v))
    case Var(n) if (n == v) => Const(1)
    case _ => Const(0)
  }

  def simplify(t: Tree): Int = t match{
    case Sum(l,r) => simplify(l) + simplify(r)
    case Const(n) => n
  }

  type Environment = String => Int
}
