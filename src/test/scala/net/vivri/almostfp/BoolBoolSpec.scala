package net.vivri.almostfp

import net.vivri.almostfp.?!.{!!, ^}
import net.vivri.almostfp.BoolBool.{Narrow, Rule}
import org.scalatest.{FreeSpec, Matchers}

class BoolBoolSpec extends FreeSpec with Matchers {

  "BoolBool" - {
    "it should respect boundaries" in {

      case object Bread extends Narrow[String] { override val rule = (x: String) => x.toLowerCase.contains("bread") }
      case object Pitt extends Narrow[String] { override val rule =  (x: String) => x.toLowerCase.contains("pitt") }

      val breadPitt = Bread & Pitt

      breadPitt ?! "abReaDpItTs" shouldBe ^(breadPitt, "abReaDpItTs")
      breadPitt ?! "PiTT BREaD"  shouldBe ^(Bread & Pitt, "PiTT BREaD")

      breadPitt ?! "pita bread" shouldBe !!(Bread & Pitt, "pita bread")
    }

    "Usage example" in {

      object ConstraintLibrary {
        case object DbIdRule extends Narrow[Int] {
          override val rule = { id: Int =>
            id >= 0 && id < 2000000
          }
        }

        case object NameRule extends Narrow[String] {
          override val rule: Rule[String] = _.matches("^[A-Z][a-zA-Z]{1,31}$")
        }

        case object BadNameRule extends Narrow[String] {
          override val rule: Rule[String] = _.toLowerCase.contains("badword")
        }

        case object ScottishLastName extends Narrow[String] {
          override val rule: Rule[String] = _.startsWith("Mc")
        }

        case object JewishLastName extends Narrow[String] {
          override val rule: Rule[String] = _ == "Cohen"
        }
      }

      import ConstraintLibrary._

      val firstName = NameRule & ~BadNameRule
      type FirstName = firstName.B ^ firstName.V

      val lastName = NameRule & ~BadNameRule & (ScottishLastName | JewishLastName)
      type LastName = lastName.B ^ lastName.V

      type DbId = DbIdRule.B ^ DbIdRule.V

      case class Person (id: Option[DbId], firstName: FirstName, lastName: LastName)

      val person =
        (DbIdRule ?! 123 lift, firstName ?! "Bilbo" lift, lastName ?! "McBeggins" lift) match {
          case (Right(id), Right(firstName), Right(lastName)) => Person(Some(id), firstName, lastName)
          case _ => throw new Exception
        }

      println (person)
      // Person(Some({ 123 ∈ DbIdRule }),{ Bilbo ∈ (NameRule & ~BadNameRule) },{ McBeggins ∈ ((NameRule & ~BadNameRule) & (ScottishLastName | JewishLastName)) })
    }
  }
}
