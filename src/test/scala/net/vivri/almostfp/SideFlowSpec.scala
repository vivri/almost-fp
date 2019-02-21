package net.vivri.almostfp

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class SideFlowSpec extends FlatSpec with Matchers {

  it should "return the same object" in {
    import SideFlow._

    val obj = Some(1)

    val exp = obj |-- println

    obj shouldBe exp
  }

  it should "maintain method equivalence" in {
    import SideFlow._

    val x = Some(1)

    (x |-- println) shouldBe (x eff println)
    (x |-- println) shouldBe (x tee println)
  }

  it should "perform the side effect" in {
    import SideFlow._

    var x = false

    Some(1) |-- { _ => x = true }

    x shouldBe true
  }

  it should "perform a side-effect on the initial object, if mutable" in {
    import SideFlow._

    var x = mutable.Set.empty[Int]

    x |-- { _ += 1 }

    x.size shouldBe 1
  }
}
