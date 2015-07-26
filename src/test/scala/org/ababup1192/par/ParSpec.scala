package org.ababup1192.par

import org.scalatest._

class ParSpec extends FlatSpec with Matchers {
  "sum function" should "return sum of Seq" in {
    val ints = IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val sum = ints.sum
    Par.sum(ints) should be(sum)
  }
}
