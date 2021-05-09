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

  def mutatePopulation(population: Population): Population =
    population.map(it => {
      if (Random.nextDouble() <= mutationRate) mutate(it) else it
    })

  private def selectGens(materialByFitness: Seq[ChromoFitness]): Population = {
    val total = materialByFitness.map(_.fitness).sum

    val material = materialByFitness
      .map(it => (it.fitness / total, it.chromosome))
      .foldLeft(Vector.empty[Tuple2[BigDecimal, Chromosome]]) { (acc, item) => {
        val sum = acc.lastOption.getOrElse(Tuple2(BigDecimal(0), None))._1 + item._1
        val rounded = sum.setScale(4, BigDecimal.RoundingMode.HALF_DOWN)
        acc :+ Tuple2(rounded, item._2)
      }}

    (0 to populationMax)
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
    var bestChromosome: ChromoFitness = ChromoFitness(calculateFitness(population(0)), population(0))
    var generations = 1

    while (bestChromosome.fitness < fitnessGoal) {
      val populationByFitness = orderByFitness(population)
      bestChromosome = populationByFitness.last
      population = mutatePopulation(selectGens(populationByFitness))

//      println(s"Generation = $generations\tfitness=${bestChromosome.fitness}\t${bestChromosome.chromosome}")
      generations = generations + 1
    }
    bestChromosome.chromosome
  }
}
