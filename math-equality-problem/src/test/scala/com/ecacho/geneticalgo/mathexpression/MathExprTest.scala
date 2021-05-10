package com.ecacho.geneticalgo.mathexpression

import org.scalatest.funsuite.AnyFunSuite
import com.ecacho.geneticalgo.mathexpression.ImplicitConversions._

class MathExprTest extends AnyFunSuite {

  test("show equation") {
    Seq(
      (Plus(2, 20) -> "2 + 20"),
      (Plus(Times(2, 3), 2) -> "2 * 3 + 2"),
      (Plus(Times("x", 3), 2) -> "x * 3 + 2"),
      (Times(Plus(2, 3), 2) -> "(2 + 3) * 2"),
      (Times(Plus(2, 3), Plus(5, 2)) -> "(2 + 3) * (5 + 2)"),
    ).foreach(myexpr => {
      val equationStr = MathExpr.show(myexpr._1)
      assert(equationStr == myexpr._2)
    })
  }


  test("solve equation") {
    implicit val map = Map.empty[String, Double]
    Seq(
      // 5 - 2 * 5
      MathEquation(Minus(5, Times(2,5)), -5),
      //0 - 2 * 5 + 10
      MathEquation(Plus(Minus(0, Times(2,5)), 10), 0),
      //2*10- 2*50
      MathEquation(Minus(Times(2, 10),  Times(2, 50)), -80),
    ).foreach(myexpr => {
      val solution = MathExpr.solve(myexpr.expression)
      assert(solution == myexpr.equality)
    })
  }

  test("get variable names") {
    Seq(
      (Plus(2, "x") -> Set("x")),
      (Plus(Times("a", "b"), Minus("x", 2)) -> Set("x", "a", "b")),
      (Plus("o", "x") -> Set("x", "o")),
      (Minus("θ",  Plus(Times(2,"α"), Minus("α", "x"))), Set("θ", "α", "x")),
    ).foreach(myexpr => {
      val varList = MathExpr.getVariableNames(myexpr._1)
      assert(varList == myexpr._2)
    })

  }
}
