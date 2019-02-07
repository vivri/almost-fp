package net.vivri.almostfp

import org.scalatest.{FlatSpec, Matchers}

class RangeSpec extends FlatSpec with Matchers {

  it should "succeed on predifined open range" in {
    object OpenRange extends Range.Open[Any]

    Range.of[OpenRange.type, Any](OpenRange)(123) shouldBe Range.In(123)
  }

  it should "succeed on user-defined open range" in {
    object OpenRange extends Range[Any] {
      override def rules: Seq[(String, Any => Boolean)] = Seq("Always" -> (_ => true))
    }

    Range.of[OpenRange.type, Any](OpenRange)(123) shouldBe Range.In(123)
  }

  it should "fail on empty range" in {
    object EmptyRange extends Range.Empty[Any]

    Range.of[EmptyRange.type, Any](EmptyRange)(123) shouldBe Range.Out(Seq(Range.Empty.text))
  }

  it should "respect boundaries" in {
    object OnlyOne extends Range[Int] {
      override def rules = Seq(
        "Positive"    -> (_ > 0),
        "Less than 2" -> (_ < 2)
      )
    }

    val onlyOne = Range.of[OnlyOne.type, Int](OnlyOne) _

    onlyOne(0) shouldBe Range.Out(Seq("Positive"))
    onlyOne(1) shouldBe Range.In(1)
    onlyOne(2) shouldBe Range.Out(Seq("Less than 2"))
  }
}
