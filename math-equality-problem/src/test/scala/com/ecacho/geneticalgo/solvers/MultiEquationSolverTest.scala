package com.ecacho.geneticalgo.solvers

import org.scalatest.funsuite.AnyFunSuite
import com.ecacho.geneticalgo.mathexpression._
import com.ecacho.geneticalgo.mathexpression.ImplicitConversions._

class MultiEquationSolverTest extends AnyFunSuite {


  test("solving xy + 2x + y = -13 and 10y - 2x = -50") {
    val equations = Seq(
      MathEquation(Plus(Times(Ref("x"), Ref("y")), Plus(Times(Literal(2), Ref("x")), Ref("y"))), -13),
      MathEquation(Minus(Times(Literal(10), Ref("y")), Times(Literal(2), Ref("x"))), -50),
    )

    equations.foreach(println)

    val solver = new MultiEquationSolver(equations)
    val solution = solver.findVariableValues()
    println(solution.map(it => s"${it._1} = ${it._2}").mkString(", "))

    assert(solution("x") == 10)
    assert(solution("y") == -3)
  }


  test("solving geometric problem") {
//    θ + 180 - 2α + 80 = 180
//    θ - 2α + α - x = -180
//    2θ - 2α = -80
    val equations = Seq(
      MathEquation(Plus(Times(2, "θ"), Minus(180, Plus(Times(2, "α"), 80))), 180),
      MathEquation(Minus("θ",  Plus(Times(2,"α"), Minus("α", "x"))), -180),
      MathEquation(Minus(Times(2, "θ"),  Times(2, "α")), -80),
    )
    equations.foreach(println)

    val solver = new MultiEquationSolver(equations)
    val solution = solver.findVariableValues()
    println(solution.map(it => s"${it._1} = ${it._2}").mkString(", "))

//    assert(solution("x") == 10)
//    assert(solution("y") == -3)
  }
}
