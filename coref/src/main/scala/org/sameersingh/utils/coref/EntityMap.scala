package org.sameersingh.utils.coref

import collection.mutable.{Set => MSet, HashSet, Map, HashMap}
import collection.Set
import org.sameersingh.utils.misc.Alphabet
import io.Source

/**
 * Class to store a clustering over a subset of mentions.
 */
class GenericEntityMap[M] {
  val entities: Map[Long, MSet[M]] = new HashMap // eId -> [mentions]
  val reverseMap: Map[M, Long] = new HashMap // mention -> eId

  def getEntities: Iterable[Set[M]] = {
    entities.values
  }

  def getEntity(mId: M): Long = {
    reverseMap(mId)
  }

  def getEntityIds: Set[Long] = {
    entities.keySet
  }

  def getMentionIds: Set[M] = {
    reverseMap.keySet
  }

  def numMentions = {
    reverseMap.size
  }

  def numEntities = {
    entities.size
  }

  def getMentions(eId: Long): Set[M] = {
    entities(eId)
  }

  def contains(mId: M): Boolean = reverseMap.contains(mId)

  def addCoreferentPair(m1: M, m2: M): Unit = {
    // if they don't exist, error!
    if (!contains(m1) || !contains(m2)) throw new Error("trying to merge mentions that are absent")
    // get the entities
    val m1e = getEntity(m1)
    val m2e = getEntity(m2)
    if (m1e == m2e) {
      // no need to do anything
    } else {
      // move all mentions in m2e to m1e
      val set = entities(m1e)
      for (mid <- getMentions(m2e)) {
        reverseMap.put(mid, m1e)
        set.add(mid)
      }
      entities.remove(m2e)
      // checks
      assert(getEntity(m1) == getEntity(m2))
      assert(!entities.contains(m2e))
    }
  }

  def addMention(mId: M, eId: Long) = {
    if (reverseMap.contains(mId)) {
      if (reverseMap(mId) == eId) {
        // already in the right entity
      } else {
        // remove the previous mention from its entity
        val oldEntity: MSet[M] = entities(reverseMap(mId))
        oldEntity.remove(mId)
      }
    }
    var s: MSet[M] = null
    if (entities.contains(eId)) {
      s = entities(eId) // get existing set of mentions
    } else {
      s = new HashSet[M] // create a set of mentions
      entities.put(eId, s)
    }
    s.add(mId)
    // update the reverse map
    reverseMap.put(mId, eId)
  }

  def clear: Unit = {
    entities.clear
    reverseMap.clear
  }

  def checkConsistency: Boolean = {
    var pass: Boolean = true
    for (mid: M <- reverseMap.keySet) {
      val entity: Set[M] = entities(reverseMap(mid))
      pass = pass && entity.contains(mid)
    }
    for (eid: Long <- entities.keys) {
      for (mid: M <- entities(eid)) {
        pass = pass && reverseMap(mid) == eid
      }
    }
    pass
  }

  override def toString: String = {
    val sb: StringBuffer = new StringBuffer
    for (entityId: Long <- entities.keySet) {
      sb.append("Entity(" + entityId + "): { ")
      for (mid: M <- entities(entityId)) {
        sb.append(" " + mid + " ")
      }
      sb.append(" }\n")
    }
    sb.toString
  }

}

class EntityMap extends GenericEntityMap[Long]

object EntityMap {
  def initToSingletons(mentions: Iterable[Long]): EntityMap = {
    val e: EntityMap = new EntityMap
    var entityIndex: Long = 0
    for (m: Long <- mentions) {
      e.addMention(m, entityIndex)
      entityIndex += 1
    }
    e
  }

  // Assume input text file with format "entity_id \t mention_id" for each line
  def readFromFile(filename: String, mentionAlphabet: Alphabet): EntityMap = {
    val map: EntityMap = new EntityMap
    val entityAlphabet: Alphabet = new Alphabet
    for (line: String <- Source.fromFile(filename).getLines) {
      if (!line.trim.equals("")) {
        val splits = line.trim.split("\\t")
        println("Adding {" + splits(1) + "} to {" + splits(0) + "}")
        val mentionId: Long = mentionAlphabet(splits(1).trim)
        if (mentionId == -1)
          println("Cannot find mention {" + splits(1).trim + "}")
        else
          map.addMention(mentionId, entityAlphabet(splits(0)))
      }
    }
    map
  }

  // Assume input text file with format "entity_id" for each line (mention)
  def readFromFile(filename: String): EntityMap = {
    val map: EntityMap = new EntityMap
    val entityAlphabet: Alphabet = new Alphabet
    var mentionId: Long = 0
    for (line: String <- Source.fromFile(filename).getLines) {
      //println("Adding {" + mentionId + "} to {" + line.trim + "}")
      map.addMention(mentionId, entityAlphabet(line.trim))
      mentionId += 1
    }
    map
  }

  // Test the pairwise mentions
  ///*
  def testPairwise: Unit = {
    val e = EntityMap.initToSingletons((0l until 12l))
    e.addCoreferentPair(0, 2)
    e.addCoreferentPair(1, 3)
    println(e)
    e.addCoreferentPair(4, 6)
    e.addCoreferentPair(5, 7)
    println(e)
    println(e.checkConsistency)
    e.addCoreferentPair(8, 10)
    e.addCoreferentPair(9, 11)
    println(e)
    e.addCoreferentPair(2, 4)
    println(e)
    println(e.checkConsistency)
    e.addCoreferentPair(6, 8)
    println(e)
    e.addCoreferentPair(3, 5)
    println(e)
    println(e.checkConsistency)
    e.addCoreferentPair(7, 9)
    println(e)
    e.addCoreferentPair(0, 1)
    println(e)
    println(e.checkConsistency)
  } //*/
}
