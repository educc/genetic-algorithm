package com.ecacho.geneticalgo.mathexpression

import scala.language.implicitConversions

object ImplicitConversions {

  implicit def strToRef(x: String) = Ref(x)
  implicit def intToLiteral(x: Int) = Literal(x)
}
