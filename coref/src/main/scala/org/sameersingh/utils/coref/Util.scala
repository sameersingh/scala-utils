package org.sameersingh.utils.coref

import scala.collection.mutable.{ArrayBuffer, Map, HashMap, Buffer}
import cc.factorie._

/**
 * @author sameer
 * @date 2/28/11
 */

object Evaluator {
  def evaluate[T <: MentionRecord](mentionList: Iterable[Mention[T]]): String = {
    val pred = new EntityMap
    val truth = new EntityMap
    for (mention: Mention[T] <- mentionList) {
      pred.addMention(mention.record.id, mention.entity.id)
      truth.addMention(mention.record.id, mention.trueEntityIndex.value)
    }
    println("Num pred entities for evaluation: "+pred.numEntities)
    println("Num pred mentions for evaluation: "+pred.numMentions)
    CorefEvaluator.evaluate(pred, truth)
  }

  def evaluateNumbers[T <: MentionRecord](mentionList: Iterable[Mention[T]]): Seq[Double] = {
    val pred = new EntityMap
    val truth = new EntityMap
    for (mention: Mention[T] <- mentionList) {
      pred.addMention(mention.record.id, mention.entity.id)
      truth.addMention(mention.record.id, mention.trueEntityIndex.value)
    }
    val pw = CorefEvaluator.Pairwise.evaluate(pred, truth)
    val b3 = CorefEvaluator.BCubed.evaluate(pred, truth)
    val muc = CorefEvaluator.MUC.evaluate(pred, truth)
    Seq(pw.f1, b3.f1, muc.f1)
  }
}

object Util {

  def sizeHistogram[T <: MentionRecord](mentions: Iterable[Mention[T]], entities: Seq[Entity[T]]): String = {
    val min = 1
    val max = entities.foldLeft(-1)((max: Int, e: Entity[T]) => math.max(max, e.size))
    val binSize = math.max((max - min) / 10, 1)
    var numBins = ((max - min) / binSize) + 1
    val counts: Array[Int] = new Array(numBins)
    entities.foreach((e: Entity[T]) => if (e.size > 0) counts((e.size - min) / binSize) += 1)
    val sb = new StringBuffer
    for (num: Int <- 0 until counts.length) {
      sb.append("%d-%d : %3d\n".format((num * binSize) + min, ((num + 1) * binSize) + min - 1, counts(num)))
    }
    sb.toString
  }

  def initToSingletons[T <: MentionRecord](mentions: Iterable[Mention[T]], entities: Seq[Entity[T]]) = {
    assert(entities.size >= mentions.size)
    var eIndex = 0
    for (mention: Mention[T] <- mentions) {
      val oentity = mention.entity
      mention.entityRef.set(entities(eIndex))(null)
      eIndex += 1
    }
    for (entity: Entity[T] <- entities) assert(entity.size <= 1)
  }

  def initToRandom[T <: MentionRecord](mentions: Iterable[Mention[T]], entities: Seq[Entity[T]]) = {
    assert(entities.size >= mentions.size)
    val numEntities: Int = random.nextInt(entities.size) + 1
    for (mention: Mention[T] <- mentions) {
      val eid: Int = random.nextInt(numEntities)
      val entity: Entity[T] = entities(eid)
      mention.entityRef.set(entity)(null)
    }
  }

  def initToTruth[T <: MentionRecord](mentions: Iterable[Mention[T]], entities: Seq[Entity[T]]) = {
    // map of teid -> eindex
    val map: Map[Long, Int] = new HashMap
    var eIndex = 0
    for (mention: Mention[T] <- mentions) {
      val eid: Long = mention.trueEntityIndex.value
      if (!map.contains(eid)) {
        map(eid) = eIndex
        eIndex += 1
      }
      val entity: Entity[T] = entities(map(eid))
      mention.entityRef.set(entity)(null)
    }
  }

  def removeEmptyEntities[T <: MentionRecord](mentions: Iterable[Mention[T]], entities: Buffer[Entity[T]], newEntity: () => Entity[T]) = {
    // map of oeid -> eindex
    val map: Map[Long, Int] = new HashMap
    entities.clear
    for (mention: Mention[T] <- mentions) {
      val eid: Long = mention.entity.id
      val entity: Entity[T] = if (map.contains(eid)) {
        entities(map(eid))
      } else {
        val e = newEntity()
        map(eid) = entities.size
        entities += e
        e
      }
      mention.entityRef.set(entity)(null)
    }
  }

  /*
  Moves all the mentions to entities in the beginning of the sequence, so that the empty ones are all at the back.
  Returns the position of the first empty entity
  */
  def moveEmptyEntitiesToBack[R <: MentionRecord](entities: Seq[Entity[R]]): Int = {
    var firstEmpty = -1
    def nextEmpty(): Int = {
      do {
        firstEmpty += 1
      } while (firstEmpty < entities.size && entities(firstEmpty).size != 0)
      math.min(firstEmpty, entities.size)
    }
    var index = -1
    def nextNonEmpty(): Int = {
      do {
        index += 1
      } while (index < entities.size && entities(index).size == 0)
      math.min(index, entities.size)
    }
    nextEmpty
    nextNonEmpty
    while (firstEmpty < entities.size && index < entities.size) {
      if (index > firstEmpty) {
        assert(entities(firstEmpty).size == 0)
        assert(entities(index).size != 0)
        for (m: Mention[R] <- entities(index).mentions) {
          m.entityRef.set(entities(firstEmpty))(null)
        }
        nextEmpty
      }
      nextNonEmpty
    }
    //nextEmpty
    firstEmpty
  }

  def main(args: Array[String]): Unit = {
    val entities: ArrayBuffer[Entity[BasicRecord]] = new ArrayBuffer
    val numFull = 4
    val numEmpty = 0
    for (i: Int <- 0 until numFull) {
      val record = new BasicRecord(i)
      val entity = new Entity[BasicRecord](i)
      val mention = new Mention(record, 0, entity)
      entities += entity
    }
    for (i: Int <- 0 until numEmpty) {
      val record = new BasicRecord(i + numFull)
      val entity = new Entity[BasicRecord](i + numFull)
      entities += entity
    }
    println(entities.toList)
    val test = entities.shuffle(cc.factorie.random)
    println(test.toList)
    moveEmptyEntitiesToBack(test)
    println(test)
  }
}