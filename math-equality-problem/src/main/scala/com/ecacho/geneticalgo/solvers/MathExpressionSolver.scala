package com.ecacho.geneticalgo.solvers

import com.ecacho.geneticalgo.ga.{Gen, GeneticAlgorithm}
import com.ecacho.geneticalgo.mathexpression._

import scala.util.Random


class MathExpressionSolver(equation: MathEquation) extends GeneticAlgorithm {

  override val populationMax = 100

  val exprVariableNameList = MathExpr.getVariableNames(equation.expression)

  private case class VarValue(num: Int) extends Gen

  override def populationGenerator(): LazyList[Chromosome] =
    LazyList.from(0).map(_ => {
      (0 until exprVariableNameList.size)
        .map(_ => VarValue(Random.between(-30, 30)))
        .toSeq
    })

  override def calculateFitness(chromosome: Chromosome): BigDecimal = {
    val refValues = exprVariableNameList
      .zip(chromosome.asInstanceOf[Seq[VarValue]])
      .map(it => (it._1 -> it._2.num.toDouble))
      .toMap

    val solveValue = MathExpr.solve(equation.expression)(refValues)

    val r: BigDecimal = 1 / (1 + (solveValue - equation.equality).abs)
    r.setScale(4, BigDecimal.RoundingMode.HALF_DOWN)
  }

  override def mutate(chromosome: Chromosome): Chromosome = {
    val idx = Random.between(0, exprVariableNameList.size)
    val increase = if (Random.between(1,3) == 1) 1 else -1
    val newValue = chromosome(idx).asInstanceOf[VarValue].num + increase
    chromosome.patch(idx, Seq(VarValue(newValue)), 1)
  }

  def findVariableValues() = {
    val best = findBest()
    exprVariableNameList
      .zip(best.asInstanceOf[Seq[VarValue]])
      .map({case (varName, value) => (varName -> value.num) })
      .toMap
  }

}


object MathExpressionSolver extends App {
  val expressions = Seq(
    (Plus(Times(Ref("x"), Literal(2)), Literal(2)), 10),
    (Plus(Ref("a"), Plus(Times(Literal(2), Ref("b")), Minus(Times(Literal(3), Ref("c")), Times(Literal(15), Ref("d"))))), 30),
  )

  expressions.foreach(myexpr => {
    val equationStr = MathExpr.show(myexpr._1) + " = " + myexpr._2
    val equation = MathEquation(myexpr._1, myexpr._2)
    val solver = new MathExpressionSolver(equation)
    val solution = solver.findVariableValues()
    println(s"$equationStr")
    println(solution.map(it => s"${it._1} = ${it._2}").mkString(", "))
    println("--------------")
  })
}