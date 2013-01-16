package org.sameersingh.utils.coref

import collection.Set
import collection.mutable.{Set => MSet, HashSet}
import org.sameersingh.utils.timing.TimeUtil

/**
 */
object CorefEvaluator {

  class Metric {
    var precNumerator = 0.0;
    var precDenominator = 0.0;
    var recallNumerator = 0.0;
    var recallDenominator = 0.0;

    def precision: Double = {
      if (precDenominator == 0.0) {
        1.0
      } else {
        precNumerator / precDenominator
      }
    }

    def recall: Double = {
      if (recallDenominator == 0.0) {
        1.0
      } else {
        recallNumerator / recallDenominator
      }
    }

    def f1: Double = {
      val r: Double = recall
      val p: Double = precision
      if(p + r == 0.0) 0.0
      else (2 * p * r) / (p + r)
    }

    def toString(prefix: String): String = {
      "%s %6.3f %6.3f %6.3f".format(prefix, precision * 100.0, recall * 100.0, f1 * 100.0)
    }

    def microAppend(m: Metric) {
      precNumerator += m.precNumerator
      precDenominator += m.precDenominator
      recallNumerator += m.recallNumerator
      recallDenominator += m.recallDenominator
    }

    def macroAppend(m: Metric) {
      precNumerator += m.precision
      precDenominator += 1.0
      recallNumerator += m.recall
      recallDenominator += 1.0
    }
  }

  def overlap[M](ent1: Set[M], ent2: Set[M]): Int = {
    var common = 0
    if (ent1.size > ent2.size) return overlap(ent2, ent1)
    for (mid: M <- ent1) {
      if (ent2.contains(mid)) {
        common += 1
      }
    }
    common
  }

  abstract class MetricEvaluator {
    def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M]): Metric
  }

  object Pairwise extends MetricEvaluator {
    override def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M]): Metric = {
      var tp = 0.0
      var fp = 0.0
      var tn = 0.0
      var fn = 0.0
      val total: Double = pred.getMentionIds.size
      var count = 0
      // go through all mentions
      for (mid: M <- pred.getMentionIds) {
        // get the clusters
        val predId = pred.getEntity(mid)
        val predCluster: Set[M] = pred.getMentions(predId)
        val trueId = truth.getEntity(mid)
        val trueCluster: Set[M] = truth.getMentions(trueId)
        // calculate overlap
        val clusterOverlap: Double = overlap(predCluster, trueCluster).doubleValue
        // update metrics
        tp += clusterOverlap - 1.0
        tn += total - predCluster.size - trueCluster.size + clusterOverlap
        fp += predCluster.size - clusterOverlap
        fn += trueCluster.size - clusterOverlap
        count += 1
        if (count % 100000 == 0) println("count: " + count)
      }
      val m: Metric = new Metric
      m.precNumerator = tp
      m.precDenominator = tp + fp
      m.recallNumerator = tp
      m.recallDenominator = tp + fn
      m
    }
  }

  object BCubed extends MetricEvaluator {
    override def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M]): Metric = {
      val m = new Metric
      m.precDenominator = pred.getMentionIds.size
      m.recallDenominator = pred.getMentionIds.size
      // go through each mention
      for (mid: M <- pred.getMentionIds) {
        // get pred and true clusters
        val predId = pred.getEntity(mid)
        val predCluster: Set[M] = pred.getMentions(predId)
        val trueId = truth.getEntity(mid)
        val trueCluster: Set[M] = truth.getMentions(trueId)
        // calculate overlap between the two
        val clusterOverlap: Int = overlap(predCluster, trueCluster)
        // add to metric
        // prec = overlap / pred.size
        m.precNumerator += clusterOverlap.doubleValue / predCluster.size.doubleValue
        // rec = overlap / truth.size
        m.recallNumerator += clusterOverlap.doubleValue / trueCluster.size.doubleValue
      }
      m
    }
  }

  object MUC extends MetricEvaluator {
    override def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M]): Metric = {
      val m: Metric = new Metric
      // Recall:
      // go through each true cluster
      for (trueId: Long <- truth.getEntityIds) {
        // find out how many unique predicted entities the mentions belong to
        val predEntities: MSet[Long] = new HashSet
        for (mid: M <- truth.getMentions(trueId)) {
          predEntities.add(pred.getEntity(mid))
        }
        // set metrics
        m.recallNumerator +=
          truth.getMentions(trueId).size - predEntities.size
        m.recallDenominator += truth.getMentions(trueId).size - 1
      }
      // Precision:
      // go through each predicted cluster
      for (predId: Long <- pred.getEntityIds) {
        // find out how many unique true entities the mentions belong to
        val trueEntities: MSet[Long] = new HashSet
        for (mid: M <- pred.getMentions(predId)) {
          trueEntities.add(truth.getEntity(mid))
        }
        // set metrics
        m.precNumerator +=
          pred.getMentions(predId).size - trueEntities.size
        m.precDenominator += pred.getMentions(predId).size - 1
      }
      m
    }
  }

  def evaluate(pred: EntityMap, truth: EntityMap, debug: Boolean = false): String = {
    val sb: StringBuffer = new StringBuffer
    sb.append("P(mentions,entities) %d %d\n".format(pred.numMentions, pred.numEntities))
    sb.append("T(mentions,entities) %d %d\n".format(truth.numMentions, truth.numEntities))
    val pw: Metric = Pairwise.evaluate(pred, truth)
    if (debug) TimeUtil.snapshot(pw.toString("PW"))
    sb.append(pw.toString("PW") + "\n")
    val muc: Metric = MUC.evaluate(pred, truth)
    if (debug) TimeUtil.snapshot(muc.toString("MUC"))
    sb.append(muc.toString("MUC") + "\n")
    val b3: Metric = BCubed.evaluate(pred, truth)
    if (debug) TimeUtil.snapshot(b3.toString("B3"))
    sb.append(b3.toString("B3"))
    sb.toString
  }

  def main(argv: Array[String]) = {
    //seperate options from arguments
    val (opts, args) = argv.partition {
      _.startsWith("@")
    }

    //turning options array into map
    val optsMap = Map() ++ opts.map {
      x =>
        val pair = x.split("@{1,2}")(1).split("=")
        if (pair.length == 1) (pair(0), "true")
        else (pair(0), pair(1))
    }

    //use the option values
    val trueFile: String = optsMap.getOrElse("true", "")
    val predFile: String = optsMap.getOrElse("pred", "")

    TimeUtil.init

    val predEntities: EntityMap = EntityMap.readFromFile(predFile)
    TimeUtil.snapshot("Pred: " + predEntities.numMentions + " / " + predEntities.numEntities)
    val trueEntities: EntityMap = EntityMap.readFromFile(trueFile)
    TimeUtil.snapshot("True: " + trueEntities.numMentions + " / " + trueEntities.numEntities)
    println(evaluate(predEntities, trueEntities))

    TimeUtil.snapshot("Done")
  }
}
