package com.ecacho.geneticalgo.solvers

import com.ecacho.geneticalgo.ga.{Gen, GeneticAlgorithm}

import scala.util.Random


case class VarGen(value: Int) extends Gen

class SimpleEqualitySolver extends GeneticAlgorithm {

  override val populationMax = 100

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

    val r = BigDecimal(1) / BigDecimal(1 + Math.abs(((a) + (2*b) + (3*c) - (15*d)) - 30))
    r.setScale(4, BigDecimal.RoundingMode.HALF_DOWN)
  }

  override def mutate(chromosome: Chromosome): Chromosome = {
    val idx = Random.between(0, 4)
    val increase = if (Random.between(1,3) == 1) 1 else -1
    val newValue = chromosome(idx).asInstanceOf[VarGen].value + increase
    chromosome.patch(idx, Seq(VarGen(newValue)), 1)
  }

}

object Main extends App {

  val obj = new SimpleEqualitySolver()
  val best = obj.findBest()
  println(s"a + 2b + 3c - 15d = 30")

  val a = best(0).asInstanceOf[VarGen].value
  val b = best(1).asInstanceOf[VarGen].value
  val c = best(2).asInstanceOf[VarGen].value
  val d = best(3).asInstanceOf[VarGen].value

  println(s"$a + 2*$b + 3*$c - 15*$d = 30")
}
