package com.ecacho.geneticalgo.mathexpression

object MathExpr {

  def getVariableNames(e: Expr): Set[String] = {
    def inner(root: Expr, acc: Set[String]): Set[String] = root match {
      case Literal(_) => Set.empty
      case Ref(x) => Set(x)
      case Times(a, b) => inner(a, acc) ++ inner(b, acc)
      case Plus(a, b) => inner(a, acc) ++ inner(b, acc)
      case Minus(a, b) => inner(a, acc) ++ inner(b, acc)
      case Divide(a, b) => inner(a, acc) ++ inner(b, acc)
    }
    inner(e, Set.empty)
  }

  def solve(e: Expr)(implicit refValues: Map[String, Double]): Double = e match {
    case Literal(x) => x
    case Ref(x) => refValues(x)
    case Plus(a, b) => solve(a) + solve(b)
    case Minus(a, b) => solve(a) - solve(b)
    case Times(a, b) => solve(a) * solve(b)
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

}
