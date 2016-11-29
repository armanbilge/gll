package org.compevol.gll

import spire.algebra.{Field, Trig}
import spire.syntax.cfor._
import spire.syntax.field._

import scala.reflect.ClassTag

final class Partition[@specialized(Float, Double) R : Field : Trig : ClassTag](val states: Int, val buffers: Int, val patterns: Int) {

  private[this] val d = Array.ofDim[R](buffers, patterns * states)
  private[this] val δ = Array.ofDim[R](buffers, patterns * states)
  private[this] val u = Array.ofDim[R](buffers, patterns * states)
  private[this] val π = Array.ofDim[R](states)
  private[this] val P = Array.ofDim[R](buffers * states * states)
  private[this] val dP = Array.ofDim[R](buffers * states * states)
  private[this] val L = Array.ofDim[R](patterns)
  private[this] val dL = Array.ofDim[R](patterns)
  private[this] val w = Array.ofDim[R](patterns)
  private[this] var μ = Field[R].one

  @inline private[this] def getMatrix(a: Array[R], i: Int)(j: Int, k: Int) = a(states * (i + states * j) + k)

  def partials_=(index: Int, sequence: Traversable[Long]): Unit = {
    require(sequence.size == patterns)
    val di = d(index)
    var i = 0
    for (s <- sequence; j <- 0 until states) {
      di(i) = if ((s >>> j) % 2 == 1)
        Field[R].one
      else
        Field[R].zero
      i += 1
    }
  }

  def probabilityMatrix_=(index: Int, derivative: Boolean = false, matrix: (Int, Int) => R): Unit = {
    var i = states * states * index
    val D = if (derivative) dP else P
    for (j <- 0 until states; k <- 0 until states) {
      D(i) = matrix(j, k)
      i += 1
    }
  }

  def frequencies_=(frequencies: Traversable[R]): Unit = {
    require(frequencies.size == patterns)
    var i = 0
    for (frequency <- frequencies) {
      π(i) = frequency
      i += 1
    }
  }

  def weights_=(weights: Traversable[R]): Unit = {
    require(weights.size == patterns)
    var i = 0
    for (weight <- weights) {
      w(i) = weight
      i += 1
    }
  }

  def mutationRate_=(rate: R): Unit = μ = rate

  def calculateΔ(nodeIndex: Int, matrixIndex: Int): Unit = {
    val x = d(nodeIndex)
    val y = δ(nodeIndex)
    val p = getMatrix(P, matrixIndex)(_: Int, _: Int)
    var l = 0
    var m = 0
    cfor(0)(_ < patterns, _ + 1) { _ =>
      cfor(0)(_ < states, _ + 1) { i =>
        y(l) = Field[R].zero
        cfor(0)(_ < states, _ + 1) { j =>
          y(l) += p(i, j) * x(m + j)
        }
        l += 1
      }
      m += states
    }
  }

  def calculateD(nodeIndex: Int, childIndices: Traversable[Int]): Unit = {
    val xs = childIndices.map(δ).toArray
    val y = d(nodeIndex)
    cfor(0)(_ < patterns, _ + 1) { i =>
      y(i) = Field[R].one
      cfor(0)(_ < xs.length, _ + 1) { j =>
        y(i) *= xs(j)(i)
      }
    }
  }

  def calculateU(nodeIndex: Int, parentIndex: Int, siblingIndices: Traversable[Int], matrixIndex: Int): Unit = {
    val x = u(nodeIndex)
    val ys = siblingIndices.map(δ).toArray
    val z = u(nodeIndex)
    val p = getMatrix(P, matrixIndex)(_: Int, _: Int)
    var l = 0
    var m = 0
    cfor(0)(_ < patterns, _ + 1) { _ =>
      cfor(0)(_ < states, _ + 1) { i =>
        z(l) = Field[R].zero
        cfor(0)(_ < states, _ + 1) { j =>
          var tmp = x(m + j) * p(j, i)
          cfor(0)(_ < ys.length, _ + 1) { k =>
            tmp *= ys(k)(m + j)
          }
          z(l) += tmp
        }
        l += 1
      }
      m += states
    }
  }

  def seedU(node1Index: Int, node2Index: Int): Unit = {
    val x = u(node1Index)
    val y = u(node2Index)
    val s = d(node1Index)
    val t = d(node2Index)
    var i = 0
    cfor(0)(_ < patterns, _ + 1) { _ =>
      cfor(0)(_ < states, _ + 1) { j =>
        x(i) = t(i) * π(j)
        y(i) = s(i) * π(j)
        i += 1
      }
    }
  }

  def calculateL(node1Index: Int, node2Index: Int, matrixIndex: Int, derivative: Boolean): Unit = {
    val x = d(node1Index)
    val y = d(node2Index)
    val p = if (derivative) getMatrix(dP, matrixIndex)(_: Int, _: Int) else getMatrix(P, matrixIndex)(_: Int, _: Int)
    val D = if (derivative) dL else L
    var l = 0
    var m = 0
    cfor(0)(_ < patterns, _ + 1) { i =>
      D(i) = Field[R].zero
      cfor(0)(_ < states, _ + 1) { j =>
        var tmp = Field[R].zero
        cfor(0)(_ < states, _ + 1) { k =>
          tmp += y(m + k) * p(j, k)
        }
        D(i) += tmp * π(j) * x(l)
        l += 1
      }
      m += states
    }
  }

  def calculateL(nodeIndex: Int, parentIndex: Int, siblingIndices: Traversable[Int], matrixIndex: Int, derivative: Boolean): Unit = {
    val x = u(parentIndex)
    val ys = siblingIndices.map(δ).toArray
    val z = d(nodeIndex)
    val p = if (derivative) getMatrix(dP, matrixIndex)(_: Int, _: Int) else getMatrix(P, matrixIndex)(_: Int, _: Int)
    val D = if (derivative) dL else L
    var l = 0
    var m = 0
    cfor(0)(_ < patterns, _ + 1) { i =>
      D(i) = Field[R].zero
      cfor(0)(_ < states, _ + 1) { j =>
        var tmp = Field[R].zero
        cfor(0)(_ < states, _ + 1) { k =>
          tmp += z(m + k) * p(j, k)
        }
        cfor(0)(_ < ys.length, _ + 1) { k =>
          tmp *= ys(k)(j)
        }
        D(i) += tmp * x(l)
        l += 1
      }
      m += states
    }
  }

  def logL: R = {
    var logL = Field[R].zero
    cfor(0)(_ < patterns, _ + 1) { i =>
      logL += w(i) * Trig[R].log(L(i))
    }
    logL
  }

  def dLogL: R = {
    var dLogL = Field[R].zero
    cfor(0)(_ < patterns, _ + 1) { i =>
      dLogL += w(i) * dL(i) / L(i)
    }
    dLogL
  }

}
