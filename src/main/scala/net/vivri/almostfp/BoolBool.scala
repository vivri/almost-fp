package net.vivri.almostfp

import net.vivri.almostfp.?!.{^, !!}

import scala.util.Try

/**
  * BoolBool is a small library, which applies boolean algebra to the problem of constraint satisfaction
  * in a type-safe and fluent manner.
  *
  * Each `BoolBool[T]` is an expression, which is comprised of combining `BoolBool.Constraint[T]`s using
  * adapted boolean operators
  *
  * `~_`      - negates the `BoolBool[T]`
  * `_ & _`   - ANDs the `BoolBool[T]` with another
  * `_ | _`   - ORs the `BoolBool[T]` with another
  * `_ X _`   - XORs the `BoolBool[T]` with another
  *
  * Therefore, you are able to express constraints in the following manner:
  *`
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
  *`
  */

sealed trait BoolBool[T] {
  type V = T
  type B <: BoolBool[T]

  def unary_~ : BoolBool[T]

  def | (expr: BoolBool[T]): BoolBool[T]

  def & (expr: BoolBool[T]): BoolBool[T]

  def X (expr: BoolBool[T]): BoolBool[T]

  def ?! (v: T): ?![B,T]
}

object BoolBool {
  type Rule[T] = T => Boolean

  trait Expr[T] extends BoolBool[T] {
    def unary_~ : BoolBool[T] = NOT(this)

    def | (b: BoolBool[T]): BoolBool[T] = b match {
      case e: Expr[T] => OR(this, e)
      case c: Narrow[T] => OR(this, ID(c))
    }

    def & (b: BoolBool[T]): BoolBool[T] = b match {
      case e: Expr[T] => AND(this, e)
      case c: Narrow[T] => AND(this, ID(c))
    }

    def X (b: BoolBool[T]): BoolBool[T] = b match {
      case e: Expr[T] => XOR(this, e)
      case c: Narrow[T] => XOR(this, ID(c))
    }

    val pprint: String

    override def toString(): String = pprint

    override def equals(o: scala.Any): Boolean =
      Try(o.asInstanceOf[Expr[T]].pprint == pprint) getOrElse false

    override def hashCode(): Int = pprint.hashCode
  }

  trait Narrow[T] extends BoolBool[T] {

    override type B = this.type

    private val id = ID(this)

    val rule: BoolBool.Rule[T]

    override def ?! (value: T): ?![B,T] = rule(value) match {
      case true  => ^(this, value)
      case false => !!(this, value)
    }

    def unary_~ : BoolBool[T] = NOT(id)

    def | (b: BoolBool[T]): BoolBool[T] = b match {
      case e: Expr[T] => OR(id, e)
      case c: Narrow[T] => OR(id, ID(c))
    }

    def & (b: BoolBool[T]): BoolBool[T] = b match {
      case e: Expr[T] => AND(id, e)
      case c: Narrow[T] => AND(id, ID(c))
    }

    def X (b: BoolBool[T]): BoolBool[T] = b match {
      case e: Expr[T] => XOR(id, e)
      case c: Narrow[T] => XOR(id, ID(c))
    }

    lazy val pprint = id.pprint
  }

  case class T[Q]() extends Expr[Q] {
    override type B = this.type
    override def ?!(v: Q) = ^(this, v)
    override val pprint: String = "Q"
  }

  case class F[Q]() extends Expr[Q] {
    override type B = this.type
    override def ?!(v: Q) = !!(this, v)
    override val pprint: String = "F"
  }

  case class ID[T](constraint: Narrow[T]) extends Expr[T] {
    override type B = this.type
    override def ?!(v: T) =
      if (constraint rule v) ^(this, v)
      else                   !!(this, v)

    override val pprint: String = constraint.toString
  }

  case class NOT[T](expr: Expr[T]) extends Expr[T] {
    override type B = this.type
    override def ?!(v: T) = {
      expr ?! v match {
        case ^(_,v)   => !!(this, v)
        case !!(_,v) => ^(this, v)
      }
    }

    override val pprint: String = s"~${expr.pprint}"
  }

  case class OR[T](exprA: Expr[T], exprB: Expr[T]) extends Expr[T] {
    override type B = this.type
    override def ?!(v: T) =
      (exprA ?! v, exprB ?! v) match {
        case (!!(_,_), !!(_,_)) =>
          !!(this, v)
        case _ =>
          ^(this,v)
      }

    override val pprint: String = s"(${exprA.pprint} | ${exprB.pprint})"
  }

  case class AND[T](exprA: Expr[T], exprB: Expr[T]) extends Expr[T] {
    override type B = this.type
    override def ?!(v: T) =
      (exprA ?! v, exprB ?! v) match {
        case (^(_,_), ^(_,_)) =>
          ^(this, v)
        case _ =>
          !!(this,v)
      }

    override val pprint: String = s"(${exprA.pprint} & ${exprB.pprint})"
  }

  case class XOR[T](exprA: Expr[T], exprB: Expr[T]) extends Expr[T] {
    override type B = this.type
    override def ?!(v: T) = {
      (exprA ?! v, exprB ?! v) match {
        case (^(_,_), ^(_,_)) =>
          !!(this, v)
        case (!!(_,_), !!(_,_)) =>
          !!(this, v)
        case _ =>
          ^(this,v)
      }
    }

    override val pprint: String = s"(${exprA.pprint} ⊕ ${exprB.pprint})"
  }
}

/**
  * Represents the application of a BoolBool algebraic expression to a value.
  */
sealed trait ?![B <: BoolBool[T], T] {
  val constraint: B
  val value: T
  val pprint: String
  val isMember: Boolean

  lazy val lift : Either[!![B,T], ^[B,T]] = isMember match {
    case true => Right(^(constraint,value))
    case false => Left(!!(constraint,value))
  }

  override def toString: String = pprint

  override def equals(o: scala.Any): Boolean =
    Try { o.asInstanceOf[?![B,T]].pprint == pprint } getOrElse false

  override def hashCode(): Int = pprint.hashCode
}

object ?! {

  def apply[B <: BoolBool[T], T](constraint: B, value: T): ?![constraint.B, T] = constraint ?! value

  def unapply[B <: BoolBool[T], T](arg: ?![B,T]): Option[(B,T)] = Some((arg.constraint,arg.value))

  case class ^[B <: BoolBool[T], T] private[?!] (constraint: B, value: T) extends ?![B, T] {
    override val pprint: String = s"{ $value ∈ ${constraint.toString} }"
    override val isMember: Boolean = true
  }

  case class !![B <: BoolBool[T], T] private[?!] (constraint: B, value: T) extends ?![B, T] {
    override val pprint: String = s"{ $value ∉ ${constraint.toString} }"
    override val isMember: Boolean = false
  }
}
