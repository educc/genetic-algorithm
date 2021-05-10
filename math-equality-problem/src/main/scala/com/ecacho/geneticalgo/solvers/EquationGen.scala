package com.ecacho.geneticalgo.solvers

import com.ecacho.geneticalgo.ga.Gen

case class EquationGen(number: Int) extends Gen {

  override def toString: String = number.toString
}
