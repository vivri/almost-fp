# almost-fp

This is a growing eclectic collection of utilities/libraries, which I was personally missing in my 
Scala escapades.

## Who is this for?

It is for the unwashed masses of Scala developers, who, like me, wince every time we see Java written
in Scala, however haven't yet reached the level of "thinking functionally", which Typelevel or Scalaz 
necessitate.

However, I will be ~~pleasantly surprised~~ happy if the FP crowd will find it useful as well.

## What's in here?

### [SideFlow](https://github.com/vivri/almost-fp/blob/master/src/main/scala/net/vivri/almostfp/SideFlow.scala)

Ever had to write things like:
```scala
def ontoSomething() {
    val something = Some("thing")
    save_in_db (something)
    log (something)
    something
}
```

_NEVER AGAIN!_

Enter `SideFlow`:
```scala
def ontoSomething() = Some("thing") |-- save_in_db |-- log
```

`SideFlow` turns side-effecting statements into side-effecting expressions - just short of it being a Monad.

Most importantly, __it saves you lines, and makes things pretty__!

There is alternative syntax for Linux-lovers (`tee`) and for symbol-haters (`eff`).

### [Narrow](https://github.com/vivri/almost-fp/blob/master/src/main/scala/net/vivri/almostfp/Narrow.scala)

Scala [does not have full-fledged dependent-types](https://www.infoq.com/presentations/scala-idris),
so we cannot natively have the niceness of `PositiveInteger`, `NonEmptyList`, and `StringOfLength19StartingWithXXX`.

* There is a pretty great library called [refined](https://github.com/fthomas/refined), which pretty much does that.
So why bother with anything else? There is a pretty big difference in both syntax and amount of background knowledge
in type theory, and Scala wizardry.

------

To get back to our issue, when we write our domain, we mostly use the raw types ~~god~~  Martin & Co. gave us. 
For example:
```scala
// We know that the `id` is non-negative, and that `name` is always capitalized, 
// and is between 2 and 22 characters.
case class Person private (id: Long, name: String)

object Person {
  def apply(id: Long, name: String): Person = validate(new Person(id, name))

  def validate(p: Person): Person = {
    if (p.id < 0 || p.name.matches(some_regex))
      throw new IllegalArgumentException("Blah!")
    else
      p
  }
}

```

Great, right? __WRONG!__

We may start defensively validating it across service-boundaries, not knowing whether it's been validated before.

Worse, once we've validated (procedurally), the domain knowledge of the _narrow_ types of `id, name` is forever 
lost to us. This is information that could, _should_ live on the type level!

__`Narrow` to the rescue!__

`Narrow` allows us to declare business-rules using composable Boolean Algebra, check them once, and 
_tie them to a type_, which will forever live in the codebase, as compiled documentation of the specification!

Our example, with a few pretty cool rules thrown in there to showcase the algebra:
```scala
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

  type DbId = DbIdRule.B ^ DbIdRule.V

  val firstName = NameRule & ~BadNameRule
  type FirstName = firstName.B ^ firstName.V

  val lastName = firstName & (ScottishLastName | JewishLastName)
  type LastName = lastName.B ^ lastName.V

  case class Person (id: Option[DbId], firstName: FirstName, lastName: LastName)

  val person =
    (DbIdRule ?! 123 lift, firstName ?! "Bilbo" lift, lastName ?! "McBeggins" lift) match {
      case (Right(id), Right(firstName), Right(lastName)) => Person(Some(id), firstName, lastName)
      case _ => throw new Exception
    }
    
  println (person)
  // Person(Some({ 123 ∈ DbIdRule }),{ Bilbo ∈ (NameRule & ~BadNameRule) },{ McBeggins ∈ ((NameRule & ~BadNameRule) & (ScottishLastName | JewishLastName)) })
```

Is it much more verbose? _You bet!_

Is it much more powerful, composable, and serves as living documentation for posterity? _Unequivocally, YES!_