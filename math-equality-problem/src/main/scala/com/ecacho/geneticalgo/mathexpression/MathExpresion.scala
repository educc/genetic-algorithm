package com.ecacho.geneticalgo.mathexpression

sealed abstract class Expr {
}
abstract class TwoNumExpr extends Expr

final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends TwoNumExpr
final case class Minus(a: Expr, b: Expr) extends TwoNumExpr
final case class Times(a: Expr, b: Expr) extends TwoNumExpr
final case class Divide(a: Expr, b: Expr) extends TwoNumExpr

final case class MathEquation(expression: Expr, equality: BigDecimal) {

  def getVariableNames() = MathExpr.getVariableNames(expression)

  override def toString: String = MathExpr.show(expression) + " = " + equality
}

