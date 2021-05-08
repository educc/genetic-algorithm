package com.ecacho.mathequalityproblem

import scala.util.Random


class SimpleEqualitySolver extends GeneticAlgorithm {

  override val populationMax = 100
  case class VarGen(value: Int) extends Gen

  override def populationGenerator(): LazyList[Chromosome] =
    LazyList.from(0).map(_ => {
      (0 until 4)
        .map(_ => VarGen(Random.between(1, 30)))
        .toSeq
    })

  override def calculateFitness(chromosome: Chromosome): BigDecimal = {
    val a = chromosome(0).asInstanceOf[VarGen].value
    val b = chromosome(1).asInstanceOf[VarGen].value
    val c = chromosome(2).asInstanceOf[VarGen].value
    val d = chromosome(3).asInstanceOf[VarGen].value

    val r = BigDecimal(1) / BigDecimal(1 + Math.abs((a + 2*b + 3*c + 4*d) - 30))
    r.setScale(4, BigDecimal.RoundingMode.HALF_DOWN)
  }

  override def mutate(chromosome: Chromosome): Chromosome = {
    val newValue = VarGen(Random.between(0,30))
    val idx = Random.between(0, 4)
    chromosome.patch(idx, Seq(newValue), 1)
  }

}

object Main extends App {

  val obj = new SimpleEqualitySolver()
  obj.findBest()
}
