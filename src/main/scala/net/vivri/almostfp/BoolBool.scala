package net.vivri.almostfp

import net.vivri.almostfp.?^.{^, ~^}

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
  */

sealed trait BoolBool[T] {
  /**
    * The type of the value
    */
  type V = T

  /**
    * The precise type of this instance
    */
  type B <: BoolBool[T]

  /**
    * The precise type of success of evaluating this type against a value
    */
  type ^^ = ^[this.B,this.V]

  /**
    * The precise type of failure of evaluating this type against a value
    */
  type ~~ = ~^[this.B,this.V]

  /**
    * Negate/invert the type constraints
    *
    * Can be used without the `.`, e.g.: `~ThisRule`
    */
  def unary_~ : BoolBool[T]

  /**
    * Or/Union with another set of type constraints over the same domain
    */
  def | (expr: BoolBool[T]): BoolBool[T]

  /**
    * And/Intersect with another set of type constraints over the same domain
    */
  def & (expr: BoolBool[T]): BoolBool[T]

  /**
    * XOR with another set of type constraints over the same domain
    */
  def X (expr: BoolBool[T]): BoolBool[T]

  /**
    * Validate a value against the constraints.
    *
    * The result should either be `this.^^` if successful, or `this.~~` if failed
    */
  def ?^ (v: T): ?^[B,T]
}

object BoolBool {
  type Rule[T] = T => Boolean

  trait Expr[T] extends BoolBool[T] {
    def unary_~ : BoolBool[T] = NOT(this)

    def | (b: BoolBool[T]): BoolBool[T] = b match {
      case e: Expr[T] => OR(this, e)
      case c: ><[T] => OR(this, ID(c))
    }

    def & (b: BoolBool[T]): BoolBool[T] = b match {
      case e: Expr[T] => AND(this, e)
      case c: ><[T] => AND(this, ID(c))
    }

    def X (b: BoolBool[T]): BoolBool[T] = b match {
      case e: Expr[T] => XOR(this, e)
      case c: ><[T] => XOR(this, ID(c))
    }

    val pprint: String

    override def toString(): String = pprint

    override def equals(o: scala.Any): Boolean =
      Try(o.asInstanceOf[Expr[T]].pprint == pprint) getOrElse false

    override def hashCode(): Int = pprint.hashCode
  }

  class ><[T](val rule: BoolBool.Rule[T]) extends BoolBool[T] {

    override type B = this.type

    private val id = ID(this)

    override def ?^ (value: T): ?^[B,T] = rule(value) match {
      case true  => ^(this, value)
      case false => ~^(this, value)
    }

    def unary_~ : BoolBool[T] = NOT(id)

    def | (b: BoolBool[T]): BoolBool[T] = b match {
      case e: Expr[T] => OR(id, e)
      case c: ><[T] => OR(id, ID(c))
    }

    def & (b: BoolBool[T]): BoolBool[T] = b match {
      case e: Expr[T] => AND(id, e)
      case c: ><[T] => AND(id, ID(c))
    }

    def X (b: BoolBool[T]): BoolBool[T] = b match {
      case e: Expr[T] => XOR(id, e)
      case c: ><[T] => XOR(id, ID(c))
    }

    lazy val pprint = id.pprint
  }

  case class T[Q]() extends Expr[Q] {
    override type B = this.type
    override def ?^(v: Q) = ^(this, v)
    override val pprint: String = "Q"
  }

  case class F[Q]() extends Expr[Q] {
    override type B = this.type
    override def ?^(v: Q) = ~^(this, v)
    override val pprint: String = "F"
  }

  case class ID[T](constraint: ><[T]) extends Expr[T] {
    override type B = this.type
    override def ?^(v: T) =
      if (constraint rule v) ^(this, v)
      else                   ~^(this, v)

    override val pprint: String = constraint.toString
  }

  case class NOT[T](expr: Expr[T]) extends Expr[T] {
    override type B = this.type
    override def ?^(v: T) = {
      expr ?^ v match {
        case ^(_,v)  => ~^(this, v)
        case ~^(_,v) => ^(this, v)
      }
    }

    override val pprint: String = s"~${expr.pprint}"
  }

  case class OR[T](exprA: Expr[T], exprB: Expr[T]) extends Expr[T] {
    override type B = this.type
    override def ?^(v: T) =
      (exprA ?^ v, exprB ?^ v) match {
        case (~^(_,_), ~^(_,_)) =>
          ~^(this, v)
        case _ =>
          ^(this,v)
      }

    override val pprint: String = s"(${exprA.pprint} | ${exprB.pprint})"
  }

  case class AND[T](exprA: Expr[T], exprB: Expr[T]) extends Expr[T] {
    override type B = this.type
    override def ?^(v: T) =
      (exprA ?^ v, exprB ?^ v) match {
        case (^(_,_), ^(_,_)) =>
          ^(this, v)
        case _ =>
          ~^(this,v)
      }

    override val pprint: String = s"(${exprA.pprint} & ${exprB.pprint})"
  }

  case class XOR[T](exprA: Expr[T], exprB: Expr[T]) extends Expr[T] {
    override type B = this.type
    override def ?^(v: T) = {
      (exprA ?^ v, exprB ?^ v) match {
        case (^(_,_), ^(_,_)) =>
          ~^(this, v)
        case (~^(_,_), ~^(_,_)) =>
          ~^(this, v)
        case _ =>
          ^(this,v)
      }
    }

    override val pprint: String = s"(${exprA.pprint} ⊕ ${exprB.pprint})"
  }
}

/**
  * Represents the result of an application of a [BoolBool] algebraic type expression to a value.
  */
sealed trait ?^[B <: BoolBool[T], T] {
  val constraint : B
  val * : T
  val pprint: String

  lazy val lift : Either[~^[B,T], ^[B,T]] = this match {
    case x: ^[B,T] => Right(x)
    case x: ~^[B,T] => Left(x)
  }

  def ~ [BB <: BoolBool[TT], TT] (next: ?^[BB,TT]): Either[Set[~^[_,_]],(^[B,T], ^[BB,TT])] =
    (this, next) match {
      case (a: ^[B, T], b: ^[BB, TT]) => Right((a, b))
      case (a,b) => Left(?^.failAsSet(a) ++ ?^.failAsSet(b))
    }

  override def toString: String = pprint

  override def equals(o: scala.Any): Boolean =
    Try { o.asInstanceOf[?^[B,T]].pprint == pprint } getOrElse false

  override def hashCode(): Int = pprint.hashCode
}

object ?^ {

  def apply[B <: BoolBool[T], T](constraint: B, value: T): ?^[constraint.B, T] = constraint ?^ value

  def unapply[B <: BoolBool[T], T](arg: ?^[B,T]): Option[(B,T)] = Some((arg.constraint,arg.*))

  case class ^[B <: BoolBool[T], T] private[?^] (constraint : B, * : T) extends ?^[B, T] {
    override val pprint: String = s"{ ${*} ∈ ${constraint.toString} }"
  }

  case class ~^[B <: BoolBool[T], T] private[?^] (constraint : B, * : T) extends ?^[B, T] {
    override val pprint: String = s"{ ${*} ∉ ${constraint.toString} }"
  }

  private def failAsSet[B <: BoolBool[T], T] (x: ?^[B,T]): Set[~^[_,_]] = x match {
    case f: ~^[B,T] => Set(f)
    case _          => Set.empty
  }

  object DSL {
    // Generated from BoolBoolSpec

    implicit class Tup2[B1 <: BoolBool[T1],B2 <: BoolBool[T2],T1,T2] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2])]) {
      def ~ [B3 <: BoolBool[T3], T3] (next: ?^[B3,T3]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3])] =
        (v, next) match {
          case (Right((a,b)), c: ^[B3,T3]) => Right((a,b,c))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup3[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],T1,T2,T3] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3])]) {
      def ~ [B4 <: BoolBool[T4], T4] (next: ?^[B4,T4]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4])] =
        (v, next) match {
          case (Right((a,b,c)), d: ^[B4,T4]) => Right((a,b,c,d))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup4[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],T1,T2,T3,T4] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4])]) {
      def ~ [B5 <: BoolBool[T5], T5] (next: ?^[B5,T5]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5])] =
        (v, next) match {
          case (Right((a,b,c,d)), e: ^[B5,T5]) => Right((a,b,c,d,e))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup5[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],T1,T2,T3,T4,T5] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5])]) {
      def ~ [B6 <: BoolBool[T6], T6] (next: ?^[B6,T6]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6])] =
        (v, next) match {
          case (Right((a,b,c,d,e)), f: ^[B6,T6]) => Right((a,b,c,d,e,f))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup6[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],T1,T2,T3,T4,T5,T6] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6])]) {
      def ~ [B7 <: BoolBool[T7], T7] (next: ?^[B7,T7]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f)), g: ^[B7,T7]) => Right((a,b,c,d,e,f,g))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup7[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],T1,T2,T3,T4,T5,T6,T7] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7])]) {
      def ~ [B8 <: BoolBool[T8], T8] (next: ?^[B8,T8]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g)), h: ^[B8,T8]) => Right((a,b,c,d,e,f,g,h))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup8[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],T1,T2,T3,T4,T5,T6,T7,T8] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8])]) {
      def ~ [B9 <: BoolBool[T9], T9] (next: ?^[B9,T9]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h)), i: ^[B9,T9]) => Right((a,b,c,d,e,f,g,h,i))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup9[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],B9 <: BoolBool[T9],T1,T2,T3,T4,T5,T6,T7,T8,T9] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9])]) {
      def ~ [B10 <: BoolBool[T10], T10] (next: ?^[B10,T10]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i)), j: ^[B10,T10]) => Right((a,b,c,d,e,f,g,h,i,j))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup10[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],B9 <: BoolBool[T9],B10 <: BoolBool[T10],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10])]) {
      def ~ [B11 <: BoolBool[T11], T11] (next: ?^[B11,T11]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j)), k: ^[B11,T11]) => Right((a,b,c,d,e,f,g,h,i,j,k))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup11[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],B9 <: BoolBool[T9],B10 <: BoolBool[T10],B11 <: BoolBool[T11],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11])]) {
      def ~ [B12 <: BoolBool[T12], T12] (next: ?^[B12,T12]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k)), l: ^[B12,T12]) => Right((a,b,c,d,e,f,g,h,i,j,k,l))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup12[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],B9 <: BoolBool[T9],B10 <: BoolBool[T10],B11 <: BoolBool[T11],B12 <: BoolBool[T12],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12])]) {
      def ~ [B13 <: BoolBool[T13], T13] (next: ?^[B13,T13]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l)), m: ^[B13,T13]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup13[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],B9 <: BoolBool[T9],B10 <: BoolBool[T10],B11 <: BoolBool[T11],B12 <: BoolBool[T12],B13 <: BoolBool[T13],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13])]) {
      def ~ [B14 <: BoolBool[T14], T14] (next: ?^[B14,T14]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m)), n: ^[B14,T14]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup14[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],B9 <: BoolBool[T9],B10 <: BoolBool[T10],B11 <: BoolBool[T11],B12 <: BoolBool[T12],B13 <: BoolBool[T13],B14 <: BoolBool[T14],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14])]) {
      def ~ [B15 <: BoolBool[T15], T15] (next: ?^[B15,T15]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n)), o: ^[B15,T15]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup15[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],B9 <: BoolBool[T9],B10 <: BoolBool[T10],B11 <: BoolBool[T11],B12 <: BoolBool[T12],B13 <: BoolBool[T13],B14 <: BoolBool[T14],B15 <: BoolBool[T15],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15])]) {
      def ~ [B16 <: BoolBool[T16], T16] (next: ?^[B16,T16]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15],^[B16,T16])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)), p: ^[B16,T16]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup16[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],B9 <: BoolBool[T9],B10 <: BoolBool[T10],B11 <: BoolBool[T11],B12 <: BoolBool[T12],B13 <: BoolBool[T13],B14 <: BoolBool[T14],B15 <: BoolBool[T15],B16 <: BoolBool[T16],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15],^[B16,T16])]) {
      def ~ [B17 <: BoolBool[T17], T17] (next: ?^[B17,T17]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15],^[B16,T16],^[B17,T17])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)), q: ^[B17,T17]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup17[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],B9 <: BoolBool[T9],B10 <: BoolBool[T10],B11 <: BoolBool[T11],B12 <: BoolBool[T12],B13 <: BoolBool[T13],B14 <: BoolBool[T14],B15 <: BoolBool[T15],B16 <: BoolBool[T16],B17 <: BoolBool[T17],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15],^[B16,T16],^[B17,T17])]) {
      def ~ [B18 <: BoolBool[T18], T18] (next: ?^[B18,T18]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15],^[B16,T16],^[B17,T17],^[B18,T18])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)), r: ^[B18,T18]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup18[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],B9 <: BoolBool[T9],B10 <: BoolBool[T10],B11 <: BoolBool[T11],B12 <: BoolBool[T12],B13 <: BoolBool[T13],B14 <: BoolBool[T14],B15 <: BoolBool[T15],B16 <: BoolBool[T16],B17 <: BoolBool[T17],B18 <: BoolBool[T18],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15],^[B16,T16],^[B17,T17],^[B18,T18])]) {
      def ~ [B19 <: BoolBool[T19], T19] (next: ?^[B19,T19]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15],^[B16,T16],^[B17,T17],^[B18,T18],^[B19,T19])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)), s: ^[B19,T19]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup19[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],B9 <: BoolBool[T9],B10 <: BoolBool[T10],B11 <: BoolBool[T11],B12 <: BoolBool[T12],B13 <: BoolBool[T13],B14 <: BoolBool[T14],B15 <: BoolBool[T15],B16 <: BoolBool[T16],B17 <: BoolBool[T17],B18 <: BoolBool[T18],B19 <: BoolBool[T19],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15],^[B16,T16],^[B17,T17],^[B18,T18],^[B19,T19])]) {
      def ~ [B20 <: BoolBool[T20], T20] (next: ?^[B20,T20]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15],^[B16,T16],^[B17,T17],^[B18,T18],^[B19,T19],^[B20,T20])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)), t: ^[B20,T20]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup20[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],B9 <: BoolBool[T9],B10 <: BoolBool[T10],B11 <: BoolBool[T11],B12 <: BoolBool[T12],B13 <: BoolBool[T13],B14 <: BoolBool[T14],B15 <: BoolBool[T15],B16 <: BoolBool[T16],B17 <: BoolBool[T17],B18 <: BoolBool[T18],B19 <: BoolBool[T19],B20 <: BoolBool[T20],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15],^[B16,T16],^[B17,T17],^[B18,T18],^[B19,T19],^[B20,T20])]) {
      def ~ [B21 <: BoolBool[T21], T21] (next: ?^[B21,T21]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15],^[B16,T16],^[B17,T17],^[B18,T18],^[B19,T19],^[B20,T20],^[B21,T21])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)), u: ^[B21,T21]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
    implicit class Tup21[B1 <: BoolBool[T1],B2 <: BoolBool[T2],B3 <: BoolBool[T3],B4 <: BoolBool[T4],B5 <: BoolBool[T5],B6 <: BoolBool[T6],B7 <: BoolBool[T7],B8 <: BoolBool[T8],B9 <: BoolBool[T9],B10 <: BoolBool[T10],B11 <: BoolBool[T11],B12 <: BoolBool[T12],B13 <: BoolBool[T13],B14 <: BoolBool[T14],B15 <: BoolBool[T15],B16 <: BoolBool[T16],B17 <: BoolBool[T17],B18 <: BoolBool[T18],B19 <: BoolBool[T19],B20 <: BoolBool[T20],B21 <: BoolBool[T21],T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21] (v: Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15],^[B16,T16],^[B17,T17],^[B18,T18],^[B19,T19],^[B20,T20],^[B21,T21])]) {
      def ~ [B22 <: BoolBool[T22], T22] (next: ?^[B22,T22]): Either[Set[~^[_,_]], (^[B1,T1],^[B2,T2],^[B3,T3],^[B4,T4],^[B5,T5],^[B6,T6],^[B7,T7],^[B8,T8],^[B9,T9],^[B10,T10],^[B11,T11],^[B12,T12],^[B13,T13],^[B14,T14],^[B15,T15],^[B16,T16],^[B17,T17],^[B18,T18],^[B19,T19],^[B20,T20],^[B21,T21],^[B22,T22])] =
        (v, next) match {
          case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)), v: ^[B22,T22]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v))
          case (Left(fails), x) => Left(fails ++ ?^.failAsSet(x))
          case (Right(_), x)    => Left(?^.failAsSet(x))
        }
    }
  }

}
