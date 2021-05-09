package com.ecacho.geneticalgo.mathexpression

object MathExpr {

  def getVariableNames(e: Expr): Seq[String] = {
    val pattern = "[a-zA-Z]".r
    (pattern findAllIn show(e)).toList
  }

  def solve(e: Expr)(implicit refValues: Map[String, Double]): Double = e match {
    case Literal(x) => x
    case Ref(x) => refValues(x)
    case Times(a, b) => solve(a) * solve(b)
    case Plus(a, b) => solve(a) + solve(b)
    case Minus(a, b) => solve(a) - solve(b)
    case Divide(a, b) => solve(a) / solve(b)
  }

  def show(e: Expr): String = e match {
    case Literal(x) => if (x % 1 == 0) x.intValue().toString else x.toString
    case Ref(x) => x

    case Times(a: TwoNumExpr, b: TwoNumExpr) => s"(${show(a)}) * (${show(b)})"
    case Times(a: TwoNumExpr, b) => s"(${show(a)}) * ${show(b)}"
    case Times(a, b: TwoNumExpr) => s"(${show(a)}) * ${show(b)}"
    case Times(a, b) => show(a) + " * " + show(b)


    case Divide(a: TwoNumExpr, b: TwoNumExpr) => s"(${show(a)}) / (${show(b)})"
    case Divide(a: TwoNumExpr, b) => s"(${show(a)}) / ${show(b)}"
    case Divide(a, b: TwoNumExpr) => s"(${show(a)}) / ${show(b)}"
    case Divide(a, b) => show(a) + " / " + show(b)

    case Plus(a, b) => show(a) + " + " + show(b)
    case Minus(a, b) => show(a) + " - " + show(b)
  }

  def main(args: Array[String]): Unit = {
    val refValues = Map[String, Double](("x" -> 2))
    Seq(
      (Plus(Literal(2), Literal(20))),
      (Plus(Times(Literal(2), Literal(3)), Literal(2))),
      (Plus(Times(Ref("x"), Literal(3)), Literal(2))),
      (Times(Plus(Literal(2), Literal(3)), Literal(2))),
      (Times(Plus(Literal(2), Literal(3)), Plus(Literal(5), Literal(2)))),
    ).foreach(myexpr => {
      val equationStr = MathExpr.show(myexpr)
      val equationResult = MathExpr.solve(myexpr)(refValues)
      println(s"$equationStr = $equationResult")
    })
  }
}
