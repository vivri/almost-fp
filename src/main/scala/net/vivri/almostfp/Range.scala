package net.vivri.almostfp

/**
  * A somewhat lightweight, typesafe refinement/validation/constraint-satisfaction ("Range") library.
  *
  * The terminology is drawn from an analogy to the domain and range of functions,
  * where the domain here is the raw type, and the range is given by a set of rules, or constraints.
  *
  * Example usage from spec:
  *
  *    object OnlyOne extends Range[Int] {
  *      override def rules = Seq(
  *        "Positive"    -> (_ > 0),
  *        "Less than 2" -> (_ < 2)
  *      )
  *    }
  *
  *    val onlyOne = Range.of[OnlyOne.type, Int](OnlyOne) _
  *
  *    onlyOne(0) shouldBe Range.Out(Seq("Positive"))
  *    onlyOne(1) shouldBe Range.In(1)
  *    onlyOne(2) shouldBe Range.Out(Seq("Less than 2"))
  */

trait Range[T] {
  def rules: Seq[(String, T => Boolean)]
}

object Range {

  def of[B <: Range[T], T](bounds: B)(value: T): RangeBelonging[B, T] = {
    bounds.rules.foldLeft(List.empty[String]) { (acc, constraint) =>
      if (!constraint._2(value))
      constraint._1 :: acc
      else
      acc
    } match {
      case Nil      => Range.In[B, T](value)
      case failures => Range.Out[B, T](failures)
    }
  }


  sealed trait RangeBelonging[B <: Range[T], T]
  case class In[B <: Range[T], T](value: T) extends RangeBelonging[B, T]
  case class Out[B <: Range[T], T](violations: Seq[String]) extends RangeBelonging[B, T]

  /**
    * Extend to always belong
    */
  trait Open[T] extends Range[T] {
    override def rules: Seq[(String, T => Boolean)] = Seq.empty
  }

  /**
    * Extend to never belong
    */
  trait Empty[T] extends Range[T] {
    override def rules: Seq[(String, T => Boolean)] = Seq(Empty.text -> (_ => false))
  }

  object Empty {
    val text = "Never satisfy constraint"
  }

}