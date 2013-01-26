package org.sameersingh.utils.coref

import cc.factorie._
import collection.mutable.{Map, HashMap}

/** Observed features of a mention */
trait MentionRecord extends Serializable {
  def id: Int // supports only upto 2 billion mentions :)
  def defaultCanopies: scala.collection.Set[String] = Set.empty
}

case class BasicRecord(id : Int) extends MentionRecord

/** Unobserved "Entity" of a mention. Underlying observed mention is typed by its features */
class EntityRef[T <: MentionRecord](val mention: Mention[T], initialEntity: Entity[T])
      extends RefVariable[Entity[T]](initialEntity) {
  initialEntity.add(mention)(null)

  // When this mention is assigned to an entity, update the mention
  override def set(e: Entity[T])(implicit d: DiffList): Unit = {
    if (e != value) {
      if (value != null) value.remove(mention)
      e.add(mention)
      super.set(e)
    }
  }

  override def hashCode = mention.record.id

  override def equals(p: Any): Boolean = p match {
    case er: EntityRef[T] => mention.record.id == er.mention.record.id
    case _ => false
  }
}

abstract class TrueEntityIndex[T <: MentionRecord](i: Int) extends IntegerVariable(i) {
  def mention: Mention[T]
}

/** Observed variable that contains the "features" of a mention */
class Mention[T <: MentionRecord](val record: T, trueEntity: Int, initialEntity: Entity[T]) extends VarWithValue[T] {
  def value: T = record

  def domain = {
    new Error("Requesting domain of an observed variable")
    null
  }

  def setEntity(e: Entity[T])(implicit d: DiffList) = entityRef.set(e)(d)

  val trueEntityIndex = new TrueEntityIndex[T](trueEntity) {
    def mention = Mention.this
  }

  val entityRef = new EntityRef(this, initialEntity)


  def entity: Entity[T] = entityRef.value

  override def isConstant = true

  override def toString = "Mention(" + record + "==" + entityRef.value.id + ")"
}

/** A random variable for an entity, which is merely a set of Mentions */
class Entity[T <: MentionRecord](val id: Long) extends SetVariable[Mention[T]] {
  def mentions = members

  def isEmpty = mentions.isEmpty

  override def toString = "Entity(" + id + "): " + mentions.toSeq.size

  override def hashCode = id.toInt

  override def equals(p: Any): Boolean = p match {
    case e: Entity[T] => id == e.id
    case _ => false
  }
}

