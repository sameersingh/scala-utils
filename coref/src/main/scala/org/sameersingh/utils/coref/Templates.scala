package org.sameersingh.utils.coref

import reflect.Manifest
import cc.factorie._
import cc.factorie.la._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

/**
 * @author sameer
 * @date 2/24/11
 */

class Objective[R <: MentionRecord](val mentions: Iterable[Mention[R]])
                                   (implicit nm1: Manifest[EntityRef[R]],
                                    nm2: Manifest[TrueEntityIndex[R]])
      extends TemplateModel {

  this += new TupleTemplateWithStatistics2[EntityRef[R], TrueEntityIndex[R]] {
    def unroll1(er: EntityRef[R]) = Factor(er, er.mention.trueEntityIndex)

    def unroll2(tei: TrueEntityIndex[R]) = Factor(tei.mention.entityRef, tei)

    def score(v1: EntityRef[R]#Value, v2: TrueEntityIndex[R]#Value) = {
      val thisMentionEntity = v1
      val thisMentionTrueEntityIndex = v2
      mentions.foldLeft(0.0)((total, m) =>
        if (m.trueEntityIndex.value == thisMentionTrueEntityIndex) {
          if (m.entityRef.value == thisMentionEntity) total + 1
          else total - 1
        } else {
          if (m.entityRef.value == thisMentionEntity) total - 1
          else total + 1
        })
    }
  }
}

/*{
 sameersAffinityVector =>
 _set(new cc.factorie.la.DenseVector(domain.dimensionDomain.size) with CategoricalsValue[String] {
   def domain = sameersAffinityVector.domain
 })
} */

abstract class AffinityVectorGetter[R <: MentionRecord] {
  def getAffinity(r1: R, r2: R): FeatureVectorVariable[String]

  def domain: CategoricalDimensionTensorDomain[String]
}

abstract class PairwiseTemplate[R <: MentionRecord](val avg: AffinityVectorGetter[R])
                                                   (implicit nm1: Manifest[EntityRef[R]],
                                                    nm2: Manifest[EntityRef[R]],
                                                    nm3: Manifest[Mention[R]],
                                                    nm4: Manifest[Mention[R]])
      extends DotTemplate4[EntityRef[R], EntityRef[R], Mention[R], Mention[R]]()(nm1, nm2, nm3, nm4) {
  var normalizer: Tensor1 = null

  def unroll1(er: EntityRef[R]): Iterable[FactorType]

  def unroll2(er: EntityRef[R]): Iterable[FactorType]

  def unroll3(mention: Mention[R]) = unroll1(mention.entityRef)

  def unroll4(mention: Mention[R]) = unroll2(mention.entityRef)

  def statistics(v1: EntityRef[R]#Value, v2: EntityRef[R]#Value, v3: Mention[R]#Value, v4: Mention[R]#Value) = avg.getAffinity(v3, v4).tensor

  //override def statisticsDomains = List(avg.domain)

  //  // skip hash checking..
  //  override def factors(difflist: DiffList): Iterable[FactorType] = {
  //    val checker: PairMap[Boolean] = new SparsePairMap(true)
  //    var result = new ListBuffer[FactorType]
  //    for (diff: Diff <- difflist; factor: FactorType <- factors(diff)) {
  //      val m1: Mention[R] = factor._3
  //      val m2: Mention[R] = factor._4
  //      if (factor eq null) throw new Error("unroll returned null Factor")
  //      else {
  //        val present: Option[Boolean] = checker.get(m1.record.id, m2.record.id)
  //        if (present.isDefined) {
  //          assert(present.get)
  //        } else {
  //          result += factor
  //          checker.put(m1.record.id, m2.record.id, true)
  //        }
  //      }
  //    }
  //    result
  //  }
  //
  //  // skip hash checking..
  //  override def factors(variables: Iterable[Variable]): Iterable[FactorType] = {
  //    if (variables.size == 1) return factors(variables.head) // Efficiently avoids the HashSet.
  //    var result = new ListBuffer[FactorType]()
  //    for (v <- variables; factor <- factors(v)) {
  //      if (factor eq null) throw new Error("unroll returned null Factor") else result += factor
  //    }
  //    result
  //  }

  lazy val weightsTensor = new DenseTensor1(avg.domain.dimensionSize)

  def calcNormalizer(mentions: Seq[Mention[R]]) = {
    normalizer = new DenseTensor1(avg.domain.dimensionSize)
    //for(i:Int <- normalizer.activeDomain) normalizer(i) = 1.0
    for (m1: Mention[R] <- mentions) {
      for (m2: Mention[R] <- mentions) {
        if (m1 != m2 && m1.record.id > m2.record.id) {
          val cav = avg.getAffinity(m1.record, m2.record)
          for ((i: Int, d: Double) <- cav.tensor.activeElements) normalizer +=(i, d)
        }
      }
    }
  }

}

/*
trait PairwiseCachedScore[R <: MentionRecord, S <: AffinityVector] extends PairwiseTemplate[R, S] {
  var lastVector: Vector = null
  var lastStat: AffinityVector = null

  var scoreCache_ : PairMap[Double] = null
  var enableScoreCache_ : Boolean = false

  var calls = 0
  var hits = 0

  def debug: String = "cache-score hits: %d/%d (%3.3f)".format(hits, calls, (hits * 100.0 / calls))

  def enableScoreCache = enableScoreCache_ = true

  def disableScoreCache = enableScoreCache_ = false

  def clearScoreCache = {
    calls = 0
    hits = 0
    if (scoreCache_ != null) scoreCache_.clear
  }

  override def statistics(values: Values) = {
    val s = avg.getAffinity(values._3, values._4)
    if (enableScoreCache_) {
      //print("Stat called..")
      lastStat = s
      lastVector = s.value
    }
    Stat(s.value)
  }

  def calcScore(s: StatisticsType) = {
    if (enableScoreCache_) hits -= 1
    if (s eq null) 0.0
    else weights match {
      case w: DenseVector => {
        w dot s.vector
      }
      case w: SparseVector => w dot s.vector
    }
  }

  override def score(s: StatisticsType) = {
    //println("scored! : " + debug)
    calls += 1
    hits += 1
    if (enableScoreCache_) {
      if (scoreCache_ == null) scoreCache_ = new DensePairMap(avg.getNumMentions, Double.NaN)
      if (s.vector eq lastVector) {
        var scoreOption: Option[Double] = scoreCache_.get(lastStat.r1.id, lastStat.r2.id)
        if (!scoreOption.isDefined || scoreOption.get.isNaN) {
          scoreCache_.put(lastStat.r1.id, lastStat.r2.id, calcScore(s))
        } else {
          /*if (math.abs(score - calcScore(s)) > 1e-9) {
            println("score  : " + score)
            println("cscore : " + calcScore(s))
            println("s : " + s)
            println("v : " + lastVector)
            println("(i=%d,j=%d,v=%f)".format(lastStat.r1.id, lastStat.r2.id,
                                              scoreCache_.get(lastStat.r1.id, lastStat.r2.id).get))
            throw new Error("score unequal!")
          } */
          scoreOption.get
        }
      } else {
        throw new Error(s.vector + "!=" + lastVector)
        calcScore(s)
      }
    }
    else calcScore(s)
  }

}
 */


class Affinity[R <: MentionRecord](avg: AffinityVectorGetter[R])
                                  (implicit nm1: Manifest[EntityRef[R]],
                                   nm2: Manifest[EntityRef[R]],
                                   nm3: Manifest[Mention[R]],
                                   nm4: Manifest[Mention[R]])
      extends PairwiseTemplate[R](avg)(nm1, nm2, nm3, nm4) {
  setFactorName("Affinity")

  def unroll1(er: EntityRef[R]): Iterable[FactorType] = {
    val factors: ArrayBuffer[FactorType] = new ArrayBuffer(er.value.size)
    var index: Int = 0
    for (other: Mention[R] <- er.value.mentions) {
      if (other.entityRef.value == er.value && er.mention.record.id != other.record.id) {
        if (er.mention.record.id > other.record.id)
          factors += Factor(er, other.entityRef, er.mention, other)
        else
          factors += Factor(other.entityRef, er, other, er.mention) // unroll 2
        index += 1
      }
    }
    assert(index == factors.length)
    factors
  }

  def unroll2(er: EntityRef[R]): Iterable[FactorType] = Nil

  // symmetric

  /*{
    val factors = new List[FactorType]
    for (other: Mention[R] <- er.value.mentions) {
      if (other.entityRef.value == er.value && er.mention.record.id != other.record.id)
        if (er.mention.record.id < other.record.id)
          factors += Factor(other.entityRef, er, other, er.mention)
    }
    factors
  }*/
}

class Repulsion[R <: MentionRecord](avg: AffinityVectorGetter[R])
                                   (implicit nm1: Manifest[EntityRef[R]],
                                    nm2: Manifest[EntityRef[R]],
                                    nm3: Manifest[Mention[R]],
                                    nm4: Manifest[Mention[R]])
      extends PairwiseTemplate[R](avg)(nm1, nm2, nm3, nm4) {
  setFactorName("Repulsion")
  var mentionList_ : Iterable[Mention[R]] = null

  def setMentions(mentionList: Iterable[Mention[R]]) = mentionList_ = mentionList

  override def factors(d: Diff) = d.variable match {
    case er: EntityRef[R] => d match {
      case er.RefVariableDiff(oldEntity: Entity[R], newEntity: Entity[R]) => {
        val otherEntity: Entity[R] = if (er.value == oldEntity) newEntity else oldEntity
        val factors = new ListBuffer[FactorType]
        for (other: Mention[R] <- otherEntity.mentions) {
          if (er.mention.record.id > other.record.id)
            factors += Factor(er, other.entityRef, er.mention, other)
          else
            factors += Factor(other.entityRef, er, other, er.mention)
        }
        factors
      }
      case _ => super.factors(d)
    }
    case _ => super.factors(d)
  }

  def unroll1(er: EntityRef[R]): Iterable[FactorType] = {
    var factors: ArrayBuffer[FactorType] = new ArrayBuffer(mentionList_.size)
    for (other: Mention[R] <- mentionList_) {
      if (other.entityRef.value != er.value) {
        if (er.mention.record.id > other.record.id)
          factors += Factor(er, other.entityRef, er.mention, other) // :: factors
        else
          factors += Factor(other.entityRef, er, other, er.mention) // :: factors
      }
    }
    factors
  }

  def unroll2(er: EntityRef[R]): Iterable[FactorType] = Nil
}

abstract class EntityVectorGetter[R <: MentionRecord] {
  def getFeatureVector(e: scala.collection.Set[Mention[R]]): FeatureVectorVariable[String]

  def domain: CategoricalDimensionTensorDomain[String]
}

class EntityTemplate[R <: MentionRecord](val evg: EntityVectorGetter[R])
                                        (implicit em: Manifest[Entity[R]])
      extends DotTemplate1[Entity[R]]()(em) {
  setFactorName("Entity")

  override def statistics(v1: Entity[R]#Value) = evg.getFeatureVector(v1).value

  lazy val weightsTensor = new DenseTensor1(evg.domain.dimensionSize)

  //override def statisticsDomains = List(evg.domain)

  // skip hash checking..
  override def factors(difflist: DiffList): Iterable[FactorType] = {
    var result = new ListBuffer[FactorType]
    for (factor <- factorsOfClass[this.Factor](difflist)) {
      if (factor eq null) throw new Error("unroll returned null Factor") else result += factor
    }
    result
  }

  // skip hash checking..
  override def factors(variables: Iterable[Var]): Iterable[FactorType] = {
    if (variables.size == 1) return factors(variables.head) // Efficiently avoids the HashSet.
    var result = new ListBuffer[FactorType]()
    for (v <- variables; factor <- factors(v)) {
      if (factor eq null) throw new Error("unroll returned null Factor") else result += factor
    }
    result
  }
}
