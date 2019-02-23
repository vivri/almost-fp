package net.vivri.almostfp

import net.vivri.almostfp.?^.{^, ~^}
import net.vivri.almostfp.Narrow.><
import org.scalatest.{FreeSpec, Matchers}

class NarrowSpec extends FreeSpec with Matchers {

  "Narrow" - {
    "it should respect boundaries" in {

      case object Bread extends ><[String] (_.toLowerCase.contains("bread"))
      case object Pitt  extends ><[String] (_.toLowerCase.contains("pitt"))

      val breadPitt = Bread & Pitt

      breadPitt ?^ "abReaDpItTs" shouldBe ^(breadPitt, "abReaDpItTs")
      breadPitt ?^ "PiTT BREaD"  shouldBe ^(Bread & Pitt, "PiTT BREaD")

      breadPitt ?^ "pita bread" shouldBe ~^(breadPitt, "pita bread")
    }
  }

  "Usage example" in {
    // First, we define the precise types that make up our domain/universe/ontology
    object OurOntology {
      // `><[T] ((t: T) => Boolean)` is the building block of our boolean type algebra
      // read `><[Int]` as `narrowing int`
      case object DbId              extends ><[Int]    ((id)=> 0 <= id && id < 2000000)
      case object Name              extends ><[String] (_.matches("^[A-Z][a-zA-Z]{1,31}$"))
      case object BadName           extends ><[String] (_.toLowerCase.contains("badword"))
      case object ScottishLastName  extends ><[String] (_ startsWith "Mc")
      case object JewishLastName    extends ><[String] (_ endsWith "berg")

      // We use boolean algebra to combine base rules into more complex rules
      val FirstNameRule = Name & ~BadName
      val LastNameRule = FirstNameRule & (ScottishLastName | JewishLastName)
    }

    import ?^.DSL._ // so we can use the convenient ~ operator
    import OurOntology._

    // Our Domain is now ready to be used in ADTs and elsewhere.
    case class Person (id: DbId.^^, firstName: FirstNameRule.^^, lastName: LastNameRule.^^)

    // We string together the inputs, to form an easily-accessible data structure:
    // Either (set of failures, tuple of successes in order of evaluation)
    val validatedInput =
      (DbId          ?^ 123) ~
      (FirstNameRule ?^ "Bilbo") ~
      (LastNameRule  ?^ "McBeggins")

    // The tupled form allows easy application to case classes
    val validPerson = validatedInput map Person.tupled

    // Using the `*` postfix notation, we can access the base types if/when we wish
    val baseTypes = validPerson map { person =>
      (person.id*, person.firstName*, person.lastName*)
    }
    baseTypes shouldBe Right((123,"Bilbo","McBeggins"))

    // Using toString gives an intuitive peek at the rule algebra
    //
    // By default, the `><` type class names get printed out - however users should feel free to override `toString`,
    // with the caveat that both `equals` and `hashCode` are (mostly) delegated to the `toString` implementation - so
    // make it unique!
    validPerson.right.get.toString shouldBe
      "Person({ 123 ∈ DbId },{ Bilbo ∈ (Name & ~BadName) },{ McBeggins ∈ ((Name & ~BadName) & (ScottishLastName | JewishLastName)) })"

    // Applying an invalid set of inputs accumulates all rules that failed
    val invalid =
      (DbId      ?^ -1) ~
      (FirstNameRule ?^ "Bilbo") ~
      (LastNameRule  ?^ "Ivanov") map Person.tupled

    // We can access the errors directly
    invalid shouldBe Left(Set(~^(DbId,-1), ~^(LastNameRule, "Ivanov")))
  }

  "Generate ~ DSL (copy and paste in ?^.DSL)" ignore {

    def genBs (i: Int) = (1 to i) map { n => s"N$n <: Narrow[T$n]" } mkString ","
    def genTs (i: Int) = (1 to i) map { n => s"T$n" } mkString ","
    def genTup (i: Int) = "(" + ( (1 to i) map { n => s"^[N$n,T$n]"} mkString ",") + ")"
    def genABC (i: Int) = (('a' to 'z') take i) mkString ","
    def letter (i: Int) = 'a' + (i-1) toChar

    for (i <- 2 to 21) {
      val j = i + 1
      println (
        s"""
           |implicit class Tup${i}[${genBs(i)},${genTs(i)}] (v: Either[Set[~^[_,_]], ${genTup(i)}]) {
           |  def ~ [N$j <: Narrow[T$j], T$j] (next: ?^[N$j,T$j]): Either[Set[~^[_,_]], ${genTup(j)}] =
           |    (v, next) match {
           |      case (Right((${genABC(i)})), ${letter(j)}: ^[N$j,T$j]) => Right((${genABC(j)}))
           |      case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
           |      case (Right(_), x)    => Left(?^.failAsSet(x))
           |    }
           |}
           |
       """.stripMargin.trim
      )
    }


  }
}
