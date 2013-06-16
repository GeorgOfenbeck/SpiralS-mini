/**
 *  SpiralS Mini - ETH Zurich
 *  Copyright (C) 2013  Alen Stojanov (astojanov@inf.ethz.ch)
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see http://www.gnu.org/licenses/.
 */

package ch.ethz.spirals.rewrites

import virtualization.lms.common._
import scala.reflect.SourceContext
import java.io.PrintWriter
import java.io.StringWriter

import ch.ethz.spirals.dsls._

trait CIROptimizer {  self =>

  val IR : CIR_DSL
  import IR._

  implicit val spos = implicitly[SourceContext]
  def isZero[T](v: T, e: DefMN[T]) = e.aev.compare(e.aev.plus(v, e.aev.zero), e.aev.zero) == 0
  def isOne [T](v: T, e: DefMN[T]) = e.aev.compare(v, e.aev.one) == 0


  class CIRTransformer extends ForwardTransformer {

    val IR: self.IR.type = self.IR

    protected var negs = Set.empty[Exp[Any]]
    protected def setNeg(sym: Sym[Any]) =  { negs = negs + sym }
    protected def isNeg[T](exp: Exp[T], e: DefMN[T] = null): Boolean = exp match {
      case s@Sym(_) => negs.contains(s)
      case Const(v) if (e != null) => (e.aev.compare(e.aev.plus(v, e.aev.zero), e.aev.zero) == -1)
      case _ => false
    }

    protected def makeTimes[T](stm: Stm, s: Sym[T], e: DefMN[T], lhs:Exp[T], rhs:Exp[T], lhs_neg: Boolean, rhs_neg: Boolean): Exp[T] = {
      implicit val (aev, mev) = (e.aev, e.mev)
      (lhs_neg, rhs_neg) match {
        case (true, false)  => setNeg(s); numeric_times(lhs, rhs)
        case (false, true)  => setNeg(s); numeric_times(lhs, rhs)
        case (true, true)   =>            numeric_times(lhs, rhs)
        case (false, false) => super.transformStm(stm).asInstanceOf[Exp[T]]
      }
    }

    protected def makeDivide[T](stm: Stm, s: Sym[T], e: DefMN[T], lhs:Exp[T], rhs:Exp[T], lhs_neg: Boolean, rhs_neg: Boolean): Exp[T] = {
      implicit val (aev, mev) = (e.aev, e.mev)
      (lhs_neg, rhs_neg) match {
        case (true, false)  => setNeg(s); numeric_divide(lhs, rhs)
        case (false, true)  => setNeg(s); numeric_divide(lhs, rhs)
        case (true, true)   =>            numeric_divide(lhs, rhs)
        case (false, false) => super.transformStm(stm).asInstanceOf[Exp[T]]
      }
    }

    protected def makeMinus[T](stm: Stm, s: Sym[T], e: DefMN[T], lhs:Exp[T], rhs:Exp[T], lhs_neg: Boolean, rhs_neg: Boolean): Exp[T] = {
      implicit val (aev, mev) = (e.aev, e.mev)
      (lhs_neg, rhs_neg) match {
        case (true, false)  => setNeg(s); numeric_plus (lhs, rhs)
        case (false, true)  =>            numeric_plus (lhs, rhs)
        case (true, true)   =>            numeric_minus(rhs, lhs)
        case (false, false) => super.transformStm(stm).asInstanceOf[Exp[T]]
      }
    }

    protected def makePlus[T](stm: Stm, s: Sym[T], e: DefMN[T], lhs:Exp[T], rhs:Exp[T], lhs_neg: Boolean, rhs_neg: Boolean): Exp[T] = {
      implicit val (aev, mev) = (e.aev, e.mev)
      (lhs_neg, rhs_neg) match {
        case (true, false)  =>            numeric_minus(rhs, lhs)
        case (false, true)  =>            numeric_minus(lhs, rhs)
        case (true, true)   => setNeg(s); numeric_plus (lhs, rhs)
        case (false, false) => super.transformStm(stm).asInstanceOf[Exp[T]]
      }
    }

    override def transformStm(stm: Stm): Exp[Any] = stm match {
      case TP(s, e@NumericTimes(a, b))  => makeTimes  (stm, s, e, this(a), this(b), isNeg(a), isNeg(b))
      case TP(s, e@NumericDivide(a, b)) => makeDivide (stm, s, e, this(a), this(b), isNeg(a), isNeg(b))
      case TP(s, e@NumericPlus(a, b))   => makePlus   (stm, s, e, this(a), this(b), isNeg(a), isNeg(b))
      case TP(s, e@NumericMinus(a, b))  => makeMinus  (stm, s, e, this(a), this(b), isNeg(a), isNeg(b))
      case TP(s, SinPI(_, _)) => super.transformStm(stm)
      case TP(s, CosPI(_, _)) => super.transformStm(stm)
      case TP(s, ArrayApply(_, idx)) => {
        assert(!isNeg(idx), "Array can not be indexed with negative index: " + s)
        super.transformStm(stm)
      }
      case TP(s, Reflect(ArrayUpdate(sym, c, s1), summary, deps) ) => {
        assert(!isNeg(s1), "Array update can not be negative: " + s)
        super.transformStm(stm)
      }
      case TP(s, Reflect(ArrayApply(_, idx), _, _)) => {
        assert(!isNeg(idx), "Array can not be indexed with negative index: " + s)
        super.transformStm(stm)
      }
      case TP(s, Reflect(ArrayNew(size), _, _)) => {
        assert(!isNeg(size), "Array creation size can not be negative: " + s)
        super.transformStm(stm)
      }
      case TP(s, Mod(s1, s2)) => {
        assert(!isNeg(s1) && !isNeg(s2), "Modulo can not have negative components: " + s)
        super.transformStm(stm)
      }
      case TP(s, Reify(s1, _, _)) => {
        assert(!isNeg(s1), "Symbol in Reify can not be negative: " + s.toString)
        super.transformStm(stm)
      }
    }
  }

  def makeConstantsPositive[B](block:(List[Sym[Any]], Block[B]))
                              (implicit mList: List[Manifest[Any]], mB: Manifest[B]) : (List[Sym[Any]], Block[B]) = {
    val transformer = new CIRTransformer {

      private def makePositive[T:Manifest](exp: Exp[T], e: DefMN[T]) : (Boolean, Exp[T]) = exp match {
        case c@Const(v) if (isNeg(c, e)) => (true, Const(e.aev.abs(v)))
        case _ => (false, exp)
      }

      override def transformStm(stm: Stm): Exp[Any] = stm match {
        case TP(s, e@NumericPlus(a, b)) => {
          val (lhs_chg, lhs) = makePositive(this(a), e)
          val (rhs_chg, rhs) = makePositive(this(b), e)
          makePlus(stm, s, e, lhs, rhs, lhs_chg || isNeg(a), rhs_chg || isNeg(b))
        }
        case TP(s, e@NumericMinus(Const(v), b)) if isZero(v, e) => {
          if ( !isNeg(b) ) setNeg(s); this(b)
        }
        case TP(s, e@NumericMinus(a, b)) => {
          val (lhs_chg, lhs) = makePositive(this(a), e)
          val (rhs_chg, rhs) = makePositive(this(b), e)
          makeMinus(stm, s, e, lhs, rhs, lhs_chg || isNeg(a), rhs_chg || isNeg(b))
        }
        case TP(s, e@NumericTimes(a, b)) => {
          val (lhs_chg, lhs) = makePositive(this(a), e)
          val (rhs_chg, rhs) = makePositive(this(b), e)
          makeTimes(stm, s, e, lhs, rhs, lhs_chg || isNeg(a), rhs_chg || isNeg(b))
        }
        case _ => super.transformStm(stm)
      }
    }
    (block._1, transformer.transformBlock(block._2))
  }

  def elimZeroArithmetics[B](block:(List[Sym[Any]], Block[B]))
                            (implicit mList: List[Manifest[Any]], mB: Manifest[B]) : (List[Sym[Any]], Block[B]) = {
    val transformer = new ForwardTransformer {
      val IR:self.IR.type = self.IR
      override def transformStm(stm: Stm): Exp[Any] = stm match {
        case TP(s, e@NumericPlus(a, b)) => (this(a), this(b)) match {
          case (Const(v1), Const(v2)) => Const(e.aev.plus(v1, v2))
          case (lhs, Const(v)) if isZero(v, e) => lhs
          case (Const(v), rhs) if isZero(v, e) => rhs
          case _ => super.transformStm(stm)
        }
        case TP(s, e@NumericMinus(a, b)) => (this(a), this(b)) match {
          case (Const(v1), Const(v2)) => Const(e.aev.minus(v1, v2))
          case (lhs, Const(v)) if isZero(v, e) => lhs
          case _ => super.transformStm(stm)
        }
        case TP(s, e@NumericTimes(a, b)) => (this(a), this(b)) match {
          case (Const(v1), Const(v2)) => Const(e.aev.times(v1, v2))
          case (_, Const(v))   if isZero(v, e) => Const(e.aev.zero)
          case (Const(v), _)   if isZero(v, e) => Const(e.aev.zero)
          case (lhs, Const(v)) if isOne(v, e)  => lhs
          case (Const(v), rhs) if isOne(v, e)  => rhs
          case _ => super.transformStm(stm)
        }
        case _ => super.transformStm(stm)
      }
    }
    (block._1, transformer.transformBlock(block._2))
  }
}

