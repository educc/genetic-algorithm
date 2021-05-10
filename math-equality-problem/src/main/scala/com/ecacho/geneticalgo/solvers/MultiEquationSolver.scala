package com.ecacho.geneticalgo.solvers

import com.ecacho.geneticalgo.ga.{Gen, GeneticAlgorithm}
import com.ecacho.geneticalgo.mathexpression._

import scala.util.Random

class MultiEquationSolver(equationList: Seq[MathEquation]) extends GeneticAlgorithm {
  assert(equationList.size >= 2)

  val variableList = equationList.flatten(it => MathExpr.getVariableNames(it.expression)).toSet

  override val populationMax = 500
  override def crossoverRate = 0.8
  override def mutationRate = 0.5
  override val fitnessGoal = equationList.size.toDouble

  private case class VarValue(num: Int) extends Gen {
    override def toString: String = num.toString
  }

  private def mkRand = Random.between(1, 360)

  override def populationGenerator(): LazyList[Chromosome] =
    LazyList.from(0).map(_ => {
      (0 until variableList.size)
        .map(_ => VarValue(mkRand))
        .toSeq
    })

  override def calculateFitness(chromosome: Chromosome): BigDecimal = {
    implicit val refValues = variableList
      .zip(chromosome.asInstanceOf[Seq[VarValue]])
      .map(it => (it._1 -> it._2.num.toDouble))
      .toMap

    val r = equationList
      .map(it => (MathExpr.solve(it.expression) -> it.equality))
      .map(it => 1 / (1 + (it._1 - it._2).abs))
      .sum

    r.setScale(4, BigDecimal.RoundingMode.HALF_DOWN)
  }

  override def mutate(chromosome: Chromosome): Chromosome = {
    val idx = Random.between(0, variableList.size)
//    val newValue = Random.between(1, 30)
    val currentValue = chromosome(idx).asInstanceOf[VarValue].num
    val factor = if (Random.nextBoolean()) -1 else 1
    val increase =  Math.abs(mkRand) * factor
    val absValue = Math.abs(currentValue + increase)
    val newValue = if (absValue == 0) 1 else absValue % 360
    chromosome.patch(idx, Seq(VarValue(newValue)), 1)
  }

  def findVariableValues() = {
    val best = findBest()
    variableList
      .zip(best.asInstanceOf[Seq[VarValue]])
      .map({case (varName, value) => (varName -> value.num) })
      .toMap
  }
}