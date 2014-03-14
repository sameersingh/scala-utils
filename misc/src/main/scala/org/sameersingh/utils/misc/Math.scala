package org.sameersingh.utils.misc

/**
 * @author sameer and riedel
 */
import java.lang.StrictMath._

object Math {

  val loge2 = log(2)
  def log2(x: Double) = log(x) / loge2

  def kl(ps: Seq[Double], qs: Seq[Double]): Double = {
    assert(ps.size == qs.size)
    (ps zip qs).map { case (p, q) => if (p == 0.0) 0.0 else p * log2(p / q) } sum
  }

  def symmKL(ps: Seq[Double], qs: Seq[Double]): Double = {
    assert(ps.size == qs.size)
    kl(ps, qs) + kl(qs, ps)
  }

  def half(xs: Seq[Double]): Seq[Double] = xs.map(x => 0.5 * x)

  def elemwiseAdd(ps: Seq[Double], qs: Seq[Double]): Seq[Double] =
    (ps zip qs).map { case (p, q) => p + q }

  def jensenShannonDivergence(ps: Seq[Double], qs: Seq[Double]): Double = {
    val ms = half(elemwiseAdd(ps, qs))
    0.5 * (kl(ps, ms) + kl(qs, ms))
  }

  def kolmogorovSmirnov(xs: Seq[Double], ys: Seq[Double]): Double = {
    assert(xs.size == ys.size)
    (xs zip ys).foldLeft((0.0,0.0,0.0))((m_sx_sy, xy) => {
      val sumx = m_sx_sy._2 + xy._1
      val sumy = m_sx_sy._3 + xy._2
      val l1 = math.abs(sumx - sumy)
      val max = math.max(m_sx_sy._1, l1)
      (max, sumx, sumy)
    })._1
  }

  def hellinger(xs: Seq[Double], ys: Seq[Double]): Double = {
    assert(xs.size == ys.size)
    sqrt(0.5 * ((xs zip ys).map { case (x, y) => pow(sqrt(x) - sqrt(y), 2) } sum))
  }

  def l1(xs: Seq[Double], ys: Seq[Double]): Double = {
    assert(xs.size == ys.size)
    (xs zip ys).map { case (x, y) => abs(x - y) } sum
  }

  def l2(xs: Seq[Double], ys: Seq[Double]): Double = {
    assert(xs.size == ys.size)
    (xs zip ys).map { case (x, y) => pow(x - y, 2) } sum
  }

  def average(ds: Seq[Double]): Double = ds.sum / (ds.size.toDouble)

  def entropy(xs: Seq[Double]): Double = {
    assert(isNormalized(xs), "calculating entropy of invalid probability distribution")
    -(xs.map(x => x * log2(x)).sum)
  }

  def isNormalized(xs: Seq[Double]): Boolean = {
    xs.sum == 1.0
  }

  def normalize(xs: Seq[Double]): Seq[Double] = {
    val Z = xs.sum
    xs.map(_ / Z)
  }

  def expAndNormalize(xs: Seq[Double]): Seq[Double] = {
    normalize(xs.map(exp(_)))
  }
}

/*
 Copyright 2009 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

/**
 * Provides some functions left out of java.lang.math.
 *
 * Borrowed from scalanlp.
 *
 * @author dlwh
 */
trait Numerics {
  /**
   * The standard digamma function. Cribbed from Radford Neal
   *
   * http://google.com/codesearch/p?hl=en#EbB356_xxkI/fbm.2003-06-29/util/digamma.c
   */
  def digamma(xx: Double) = {
    var x = xx;
    var r = 0.0;

    while (x<=5) {
      r -= 1/x;
      x += 1;
    }

    var f = 1./(x * x);
    var t = f*(-1/12.0 +
            f*(1/120.0 +
            f*(-1/252.0 +
            f*(1/240.0 +
            f*(-1/132.0 +
            f*(691/32760.0 +
            f*(-1/12.0 +
            f*3617.0/8160.0)))))));
    r + log(x) - 0.5/x + t;
  }

  private val cof =  Array(76.18009172947146, -86.50532032941677,
    24.01409824083091,-1.231739572450155,
    0.1208650973866179e-2,-0.5395239384953e-5
  );

//  /**
//   * Evaluates the log of the generalized beta function.
//   *  = \sum_a lgamma(c(a))- lgamma(c.total)
//   */
//  def lbeta[T](c: DoubleCounter[T]) = {
//    c.valuesIterator.foldLeft(-lgamma(c.total))( (acc,x)=> acc +lgamma(x));
//  }

  /**
  * Computes the log of the gamma function.
  *
  * Reference: Numerical Recipes in C
  * http://www.library.cornell.edu/nr/cbookcpdf.html
  * www.cs.berkeley.edu/~milch/blog/versions/blog-0.1.3/blog/distrib
  * @return an approximation of the log of the Gamma function * of x.  Laczos Approximation
  */
  def lgamma(x : Double) = {
    var y = x;
    var tmp = x + 5.5;
    tmp -= ((x + 0.5) * log(tmp));
    var ser = 1.000000000190015;
    var j = 0;
    while (j < 6) {
      y += 1;
      ser += (cof(j)/y);
      j +=1;
    }
    (-tmp + log(2.5066282746310005*ser / x));
  }

  private val SQRT_PI = sqrt(PI);

  private val ERF_A = 8. / (3. * PI) * (PI - 3) / (4 - PI);
  private val ERF_B = 4. / PI;

  /**
  * Approximation to the inverse ERF. Based on
  * homepages.physik.uni-muenchen.de/~Winitzki/erf-approx.pdf
  */
  def erfi(x:Double) = {
    val x2 = x*x;
    val lg1mx2 = log(1- x2);
    val c = 2 / PI / ERF_A + lg1mx2/2;
    val result =  sqrt(-c + sqrt( c*c - 1/ ERF_A * lg1mx2))
    if (x < 0) -1 * result
    else result
  }

  /**
  * 1- erf(x)
  */
  def erfc(x: Double) =  1 - erf(x);

  /**
  * approximation to the erf function, for gaussian integrals.
  */
  def erf(x: Double) = {
    val mag = 1 - gamma(.5,x*x)/sqrt(PI);
    if (x < 0) -1.0 * mag // ERF is odd.
    else mag
  }

  /**
  * Incomplete lgamma function.
  */
  def lgamma(a: Double, z:Double) = {
    var res = 0.;
    var m = 21
    while( m > 1) {
      res = ((1.0-m)*(m-1.0-a)) / (2*m -1+z -a + res);
      m -= 1
    }

    a * log(z) - z - log(1+z-a+res);
  }

  /**
  * Incomplete gamma function, the exp of lgamma(a,z)
  */
  def gamma(a: Double, z:Double) = exp(lgamma(a,z));

  /**
  * Sums together things in log space.
  * @return log(exp(a) + exp(b))
  */
  def logSumExp(a : Double, b : Double) = {
    if(a == Double.NegativeInfinity) b
    else if (b == Double.NegativeInfinity) a
    else if(a < b) b + log(1 + exp(a-b))
    else a + log(1+exp(b-a));
  }

  /**
  * Sums together things in log space.
  * @return log(\sum exp(a_i))
  */
  def logSumExp(a: Double, b:Double, c: Double*):Double ={
    logSumExp(Array(a,b) ++ c);
  }

  /**
  * Sums together things in log space.
  * @return log(\sum exp(a_i))
  */
  def logSumExp(iter:Iterator[Double], max: Double):Double = {
    var accum = 0.0;
    while(iter.hasNext) {
      val b = iter.next;
      if(b != Double.NegativeInfinity)
        accum += exp(b - max);
    }
    max + log(accum);
  }

  /**
  * Sums together things in log space.
  * @return log(\sum exp(a_i))
  */
  def logSumExp(a:Seq[Double]):Double = {
    a.length match {
      case 0 => Double.NegativeInfinity;
      case 1 => a(0)
      case 2 => logSumExp(a(0),a(1));
      case _ =>
        val m = a reduceLeft(_ max _);
        if(m.isInfinite) m
        else {
          var i = 0;
          var accum = 0.0;
          while(i < a.length) {
            accum += exp(a(i) - m);
            i += 1;
          }
          m + log(accum);
        }
    }
  }

//  import scalala.tensor.Vector;
//  /**
//  * Sums together things in log space.
//  * @return log(\sum exp(a_i))
//  */
//  def logSum(a:Vector):Double = a match {
//    case a: AdaptiveVector => logSum(a.innerVector);
//    case a: DenseVector => logSum(a.data);
//    case a: SparseVector => logSum(a.data.take(a.used));
//    case _ => logSum(a.activeValues,a.activeValues.reduceLeft(_ max _));
//  }

//  /**
//  * Sums together things in log space.
//  * @return log(\sum exp(a_i))
//  */
//  def logNormalize(a:Vector):Vector = {
//    val sum = logSum(a);
//    import scalala.Scalala.{logSum => _, _};
//    a - sum value;
//  }

  /**
  * Sums together things in log space.
  * @return log(exp(a) - exp(b))
  * requires a &gt; b
  */
  def logDiff(a : Double, b : Double) = {
    a + log(1 - exp(b-a) );
  }

  /**
   * Computes the polynomial P(x) with coefficients given in the passed in array.
   * coefs(i) is the coef for the x^i term.
   */
  def poly(coefs: Array[Double], x: Double) = {
    var i = coefs.length-1;
    var p = coefs(i);
    while (i>0) {
      i -= 1;
      p = p*x + coefs(i);
    }
    p
  }
}

object Numerics extends Numerics;