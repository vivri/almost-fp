package net.vivri.almostfp

import org.scalatest.{FlatSpec, Matchers}

class NarrowSpec extends FlatSpec with Matchers {

  it should "succeed on predifined open range" in {
    object OpenRange extends Narrow.Open[Any]

    Narrow.downTo[OpenRange.type, Any](OpenRange)(123) shouldBe Narrow.Down(123)
  }

  it should "succeed on user-defined open range" in {
    object OpenRange extends Narrow[Any] {
      override def rules: Seq[(String, Any => Boolean)] = Seq("Always" -> (_ => true))
    }

    Narrow.downTo[OpenRange.type, Any](OpenRange)(123) shouldBe Narrow.Down(123)
  }

  it should "fail on empty range" in {
    object EmptyRange extends Narrow.Empty[Any]

    Narrow.downTo[EmptyRange.type, Any](EmptyRange)(123) shouldBe Narrow.Err(Seq(Narrow.Empty.text))
  }

  it should "preserve type information of Narrow type in the result" in {
    object OpenRange extends Narrow.Open[Any]

    Narrow.downTo[OpenRange.type, Any](OpenRange)(123).isInstanceOf[Narrow.Down[OpenRange.type, Any]] shouldBe true
  }

  it should "respect boundaries" in {
    object OnlyOne extends Narrow[Int] {
      override def rules = Seq(
        "Positive"    -> (_ > 0),
        "Less than 2" -> (_ < 2)
      )
    }

    val onlyOne = Narrow.downTo[OnlyOne.type, Int](OnlyOne) _

    onlyOne(0) shouldBe Narrow.Err(Seq("Positive"))
    onlyOne(1) shouldBe Narrow.Down(1)
    onlyOne(2) shouldBe Narrow.Err(Seq("Less than 2"))
  }

  it should "pass the spec in the Narrow scaladoc" in {
    object OnlyOne extends Narrow[Int] {
      override def rules = Seq(
        "Positive"    -> (_ > 0),
        "Less than 2" -> (_ < 2)
      )
    }

    val onlyOne = Narrow.downTo[OnlyOne.type, Int](OnlyOne) _

    onlyOne(0) shouldBe Narrow.Err(Seq("Positive"))
    onlyOne(1) shouldBe Narrow.Down(1)
    onlyOne(2) shouldBe Narrow.Err(Seq("Less than 2"))

    onlyOne(1).isInstanceOf[Narrow.Down[OnlyOne.type, Int]] shouldBe true
    onlyOne(0).isInstanceOf[Narrow.Err[OnlyOne.type, Int]] shouldBe true

    onlyOne(1) match {
      case Narrow.Down(i) => i.isInstanceOf[Int] shouldBe true
      case Narrow.Err(violations) => throw new RuntimeException("BOOM!")
    }
  }
}
