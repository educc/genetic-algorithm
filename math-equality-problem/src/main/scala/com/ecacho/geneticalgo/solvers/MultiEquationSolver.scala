package com.ecacho.geneticalgo.solvers

import com.ecacho.geneticalgo.ga.{Gen, GeneticAlgorithm}
import com.ecacho.geneticalgo.mathexpression._

import scala.util.Random

class MultiEquationSolver(equationList: Seq[MathEquation]) extends GeneticAlgorithm {


  assert(equationList.size >= 2)
  //TODO: validate all equations have the same variables

  val exprVariableNameList = MathExpr.getVariableNames(equationList(0).expression)


  override val populationMax = 100
  override val fitnessGoal = equationList.size.toDouble

  private case class VarValue(num: Int) extends Gen

  override def populationGenerator(): LazyList[Chromosome] =
    LazyList.from(0).map(_ => {
      (0 until exprVariableNameList.size)
        .map(_ => VarValue(Random.between(-30, 30)))
        .toSeq
    })

  override def calculateFitness(chromosome: Chromosome): BigDecimal = {
    implicit val refValues = exprVariableNameList
      .zip(chromosome.asInstanceOf[Seq[VarValue]])
      .map(it => (it._1 -> it._2.num.toDouble))
      .toMap

    val r = equationList
      .map(it => (it.equality -> MathExpr.solve(it.expression)))
      .map(it => 1 / (1 + (it._1 - it._2).abs))
      .sum

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

object MultiEquationSolver extends App {
  val equations = Seq(
    MathEquation(Plus(Times(Ref("x"), Ref("y")), Plus(Times(Literal(2), Ref("x")), Ref("y"))), -13),
    MathEquation(Minus(Times(Literal(10), Ref("y")), Times(Literal(2), Ref("x"))), -50),
  )

  equations.foreach(println)

  val solver = new MultiEquationSolver(equations)
  val solution = solver.findVariableValues()
  println(solution.map(it => s"${it._1} = ${it._2}").mkString(", "))
}