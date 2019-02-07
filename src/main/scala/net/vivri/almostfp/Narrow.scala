package net.vivri.almostfp

/**
  * A somewhat lightweight, typesafe refinement/validation/constraint-satisfaction ("Narrowing") library.
  *
  * The benefit here is that you:
  * 1) Get to keep the type of validation you've used.
  * 2) Only calculate once, keep the results forever.
  * 3) Don't carry around references to the rules you've used to restrict it.
  *
  * Example usage from imaginary spec (passes):
  *
  *    object OnlyOne extends Narrow[Int] {
  *      override def rules = Seq(
  *        "Positive"    -> (_ > 0),
  *        "Less than 2" -> (_ < 2)
  *      )
  *    }
  *
  *    val onlyOne = Narrow.downTo[OnlyOne.type, Int](OnlyOne) _
  *
  *    onlyOne(0) shouldBe Narrow.Err(Seq("Positive"))
  *    onlyOne(1) shouldBe Narrow.Type(1)
  *    onlyOne(2) shouldBe Narrow.Err(Seq("Less than 2"))
  *
  *    onlyOne(1).isInstanceOf[Narrow.Type[OnlyOne.type, Int]] shouldBe true
  *    onlyOne(0).isInstanceOf[Narrow.Err[OnlyOne.type, Int]] shouldBe true
  *
  *    onlyOne(1) match {
  *      case Narrow.Type(i) => i.isInstanceOf[Int] shouldBe true
  *      case Narrow.Err(violations) => throw new RuntimeException("BOOM!")
  *    }
  */

trait Narrow[T] {
  def rules: Seq[(String, T => Boolean)]
}

object Narrow {

  def downTo[R <: Narrow[T], T](range: R)(value: T): Belonging[R, T] = {
    range.rules.foldLeft(List.empty[String]) { (acc, constraint) =>
      if (!constraint._2(value))
      constraint._1 :: acc
      else
      acc
    } match {
      case Nil      => Narrow.Type[R, T](value)
      case failures => Narrow.Err[R, T](failures)
    }
  }

  sealed trait Belonging[R <: Narrow[T], T]
  case class Type[R <: Narrow[T], T](value: T) extends Belonging[R, T]
  case class Err[R <: Narrow[T], T](violations: Seq[String]) extends Belonging[R, T]

  /**
    * Extend to always belong
    */
  trait Open[T] extends Narrow[T] {
    override val rules: Seq[(String, T => Boolean)] = Seq.empty
  }

  /**
    * Extend to never belong
    */
  trait Empty[T] extends Narrow[T] {
    override val rules: Seq[(String, T => Boolean)] = Seq(Empty.text -> (_ => false))
  }

  object Empty {
    val text = "Never satisfy constraint"
  }

}