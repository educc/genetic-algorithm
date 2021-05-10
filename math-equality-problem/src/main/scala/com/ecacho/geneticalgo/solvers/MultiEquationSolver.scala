package com.ecacho.geneticalgo.solvers

import com.ecacho.geneticalgo.ga.GeneticAlgorithm
import com.ecacho.geneticalgo.mathexpression._

import scala.util.Random

class MultiEquationSolver(equationList: Seq[MathEquation]) extends GeneticAlgorithm {
  assert(equationList.size >= 2)

  val variableList = equationList.flatten(it => MathExpr.getVariableNames(it.expression)).toSet

  override val populationMax = 200
  override def crossoverRate = 0.5
  override def mutationRate = 0.2
  override val fitnessGoal = equationList.size.toDouble

  private def fixValue(idx: Int, value: Int) = idx match {
    case 0 => value % 90
    case 1 => value % 90
    case 2 => Math.max(91, value % 180)
  }
  private def mkRand = Random.between(1, 90)

  override def populationGenerator(): LazyList[Chromosome] =
    LazyList.from(0).map(_ => {
      (0 until variableList.size)
        //        .map(idx => EquationGen(fixValue(idx, mkRand)))
        .map(idx => EquationGen(mkRand))
    })

  override def calculateFitness(chromosome: Chromosome): BigDecimal = {
    implicit val refValues = variableList
      .zip(chromosome.asInstanceOf[Seq[EquationGen]])
      .map(it => (it._1 -> it._2.number.toDouble))
      .toMap

    equationList
      .map(it => (MathExpr.solve(it.expression) -> it.equality))
      .map(it => 1 / (1 + (it._1 - it._2).abs))
      .sum.setScale(4, BigDecimal.RoundingMode.HALF_DOWN)
  }

  override def mutate(chromosome: Chromosome): Chromosome = MutationStrategies.increaseOne(chromosome)

  def findVariableValues() = {
    val best = findBest()
    variableList
      .zip(best.asInstanceOf[Seq[EquationGen]])
      .map({case (varName, value) => (varName -> value.number) })
      .toMap
  }
}