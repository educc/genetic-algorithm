package com.ecacho.geneticalgo.solvers

import com.ecacho.geneticalgo.ga.Gen

import scala.util.Random

object MutationStrategies {

  def increaseOne(chromosome: Seq[Gen]): Seq[Gen] = {
    val idx = Random.between(0, chromosome.size)
    val currentValue = chromosome(idx).asInstanceOf[EquationGen].number
    chromosome.patch(idx, Seq(EquationGen(currentValue + 1)), 1)
  }


//  override def mutate(chromosome: Seq[EquationGen]): Seq[EquationGen] = {
//    val idx = Random.between(0, chromosome.size)
//    //    val newValue = Random.between(1, 30)
//    val currentValue = chromosome(idx).asInstanceOf[EquationGen].number
//    val factor = if (Random.nextBoolean()) -1 else 1
//    val increase =  Math.abs(mkRand) * factor
//    val absValue = Math.abs(currentValue + increase)
//    //    val newValue = if (absValue == 0) 1 else absValue % 360
//    val newValue = fixValue(idx, absValue)
//    chromosome.patch(idx, Seq(EquationGen(newValue)), 1)
//  }
}
