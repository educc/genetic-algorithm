package com.ecacho.geneticalgo.ga

import scala.util.Random
import scala.util.control.Breaks.{break, breakable}

/**
 * Generic abstract class to handle Genetic Algorithm
 */


trait Gen

abstract class GeneticAlgorithm {

  // params
  protected def populationMax = 10
  protected def mutationRate = 0.1
  protected def crossoverRate = 0.1
  protected def fitnessGoal = 1.0

  // types
  type Population = Seq[Chromosome]
  type Chromosome = Seq[Gen]

  // data holders
  case class ChromoFitness(fitness: BigDecimal, chromosome: Chromosome)

  // abstracts
  def populationGenerator(): LazyList[Chromosome]
  def calculateFitness(chromosome: Chromosome): BigDecimal
  def mutate(chromosome: Chromosome): Chromosome

  // algorithm

  private def orderByFitness(population: Population): Seq[ChromoFitness] =
    population
      .map(it =>  ChromoFitness(calculateFitness(it), it))
      .sortBy(_.fitness)

  private def mutatePopulation(population: Population): Population =
    population.map(it => {
      if (Random.nextDouble() <= mutationRate) mutate(it) else it
    })

  protected def crossover(c1: Chromosome, c2: Chromosome): Chromosome =
    c1.zip(c2)
      .map({  case (left, right) => Random.between(0,2) match {
        case 0 => left
        case 1 => right
      }})
      .toSeq

  private def crossoverPopulation(population: Population): Population =
    population.map(it => {
      if (Random.nextDouble() <= crossoverRate)
        crossover(pickOne(population), pickOne(population))
      else it
    })

  private def pickOne(population: Population): Chromosome =
    population(Random.between(0, populationMax))

  private def selectGens(materialByFitness: Seq[ChromoFitness]): Population = {
    val total = materialByFitness.map(_.fitness).sum

    val material = materialByFitness
      .map(it => (it.fitness / total, it.chromosome))
      .foldLeft(Vector.empty[Tuple2[BigDecimal, Chromosome]]) { (acc, item) => {
        val sum = acc.lastOption.getOrElse(Tuple2(BigDecimal(0), None))._1 + item._1
        val rounded = sum.setScale(4, BigDecimal.RoundingMode.HALF_DOWN)
        acc :+ Tuple2(rounded, item._2)
      }}

    (0 until populationMax)
      .map(_ => {
        val rnd = BigDecimal(Random.nextDouble())
        var last = BigDecimal(0)
        var result = material(0)._2

        breakable {
          for( (probability, gen) <- material) {
            if (rnd >= last && rnd <= probability) {
              result = gen
              break
            }
            last = probability
          }
        }
        result
      })
  }


  def findBest(): Chromosome = {
    var population: Population = populationGenerator().take(populationMax).toSeq
    var bestEver: ChromoFitness = ChromoFitness(calculateFitness(population(0)), population(0))
    var generations = 1

    while (bestEver.fitness < fitnessGoal) {
      val populationByFitness = orderByFitness(population)
      val bestCurrent = populationByFitness.last

      val bestPopulation = selectGens(populationByFitness)
      population = mutatePopulation(crossoverPopulation(bestPopulation))

      if (bestCurrent.fitness > bestEver.fitness) {
        bestEver = bestCurrent
      }

      val msgCurrent = s"fitness=${bestCurrent.fitness}\t${bestCurrent.chromosome}"
      val msgEver = s"fitness=${bestEver.fitness}\t${bestEver.chromosome}"
//      println(s"Generation = $generations\t(ever)$msgEver\t(current)$msgCurrent")
      generations = generations + 1
    }
    bestEver.chromosome
  }
}
