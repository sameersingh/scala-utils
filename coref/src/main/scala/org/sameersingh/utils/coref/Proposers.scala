package org.sameersingh.utils.coref

import cc.factorie._
import scala.util.Random
import collection.mutable._
import reflect.Manifest
import scala.Seq
import scala.Iterable
import org.sameersingh.utils.timing.TimeUtil

object Proposers {

  abstract class CorefProposer[R <: MentionRecord](model: Model) extends MHSampler[Null](model)(random) {
    def setEntities(es: Seq[Entity[R]]): Unit

    def entities: Seq[Entity[R]]
  }

  class Simple[R <: MentionRecord](model: Model,
                                   var mentionList: Seq[Mention[R]] = Seq.empty,
                                   var entityList: Seq[Entity[R]] = Seq.empty) extends CorefProposer[R](model) {
    temperature = 0.001
    var minNumEntities = 1
    val probRemainFull = 0.8

    var firstEmpty = Util.moveEmptyEntitiesToBack(entityList)
    if (firstEmpty < entityList.size) {
      assert(entityList(firstEmpty).size == 0)
    }
    assert(entityList(firstEmpty - 1).size != 0, "fe: %d, ".format(firstEmpty) + entityList)
    var lastSource: Entity[R] = null
    var lastDest: Entity[R] = null

    def setEntities(es: Seq[Entity[R]]) = {
      entityList = es
      mentionList = es.flatMap(_.mentions).toSeq
      resetStats
    }

    def entities = entityList

    def resetStats = {
      firstEmpty = Util.moveEmptyEntitiesToBack(entityList)
      lastSource = null
      lastDest = null
    }

    override def postAcceptanceHook(logAcceptanceProb: Double, d: DiffList): Unit = {
      super.postAcceptanceHook(logAcceptanceProb, d)
      if (d.length != 0) {
        if (lastSource.size == 0) {
          // move fe-1 to lastSource
          if (lastSource != entityList(firstEmpty - 1)) {
            val mentions = entityList(firstEmpty - 1).mentions.toSeq
            for (m: Mention[R] <- mentions) {
              m.entityRef.set(lastSource)(null)
            }
          }
          assert(entityList(firstEmpty - 1).size == 0)
          firstEmpty -= 1
        }
        if (lastDest.size == 1) {
          assert(lastDest == entityList(firstEmpty))
          firstEmpty += 1
        }
        if (firstEmpty < entityList.size) {
          assert(entityList(firstEmpty).size == 0)
        }
        assert(entityList(firstEmpty - 1).size != 0)
        val numNonEmpty = firstEmpty
        val numEmpty = entityList.size - firstEmpty
      }
    }

    def propose(context: Null)(implicit difflist: DiffList): Double = {
      //println("minEntities: %d numNonEmpty: %d".format(minNumEntities, firstEmpty))
      assert(firstEmpty >= minNumEntities)
      // Pick a random mention
      val m =
        if (firstEmpty > minNumEntities)
          mentionList.sampleUniformly(cc.factorie.random)
        else {
          //we're at minimum number of entities! don't pick singleton
          mentionList.filter(_.entity.size > 1).sampleUniformly(cc.factorie.random)
        }

      lastSource = m.entity
      // Pick a random place to move it, either an existing Entity or a newly created one
      var e: Entity[R] = null
      // Pick an existing entity to move it to
      if (m.entityRef.value.size == 1 || random.nextDouble < probRemainFull) {
        var picked: Entity[R] = null
        //val s2 = entityList.filter((e: Entity[R]) => e.size > 0 && e != m.entityRef.value)
        //if (s2.size != 0) e = s2(random.nextInt(s2.size))
        if (firstEmpty > 1) {
          assert(entityList(m.entity.id.toInt) == m.entity)
          val entityPos = random.nextInt(firstEmpty - 1)
          picked = if (m.entity.id.toInt <= entityPos) entityList(entityPos + 1) else entityList(entityPos) //entityList(random.nextInt(firstEmpty))
          //if (picked == m.entity) picked = entityList(random.nextInt(firstEmpty))
          if (picked == m.entity) {
            val numNonEmpty = firstEmpty
            val numEmpty = entityList.size - firstEmpty
            println("(full,empty): " + numNonEmpty + ", " + numEmpty)
            println("entityPos: " + entityPos)
            println("m.entity : " + m.entity)
            println("picked   : " + picked)
            assert(false)
          }
          assert(picked.size != 0)
        }
        e = picked
      }
      // Pick an empty entity to move it to
      if (e == null) {
        assert(entityList(firstEmpty).size == 0)
        e = entityList(firstEmpty)
      }
      lastDest = e
      val numNonEmpty = firstEmpty
      val numEmpty = entityList.size - firstEmpty
      val bfRatio: Double =
        if (m.entity.size == 1) {
          // picked a singleton
          // if numNonEmpty == minNumEntities, we cannot be here!
          assert(numNonEmpty > minNumEntities)
          // mention selection is uniform
          val f = (1.0 / mentionList.length) * (1.0 / (numNonEmpty - 1))
          val b =
            if (numNonEmpty == minNumEntities + 1) {
              // we picked a singleton, so next config will be == minNumEntities!
              // i.e. probability we pick this mention is higher!
              val currNonSingleton = entityList.slice(0, firstEmpty).filter(_.size > 1).foldLeft(0.0)(_ + _.size)
              (1.0 / (currNonSingleton + 1)) // this mention wil also be a non-singleton now
            } else {
              // uniformly pick mention
              (1.0 / mentionList.length)
            } * (1.0 - probRemainFull) // prob we pick empty entity
          math.log(b / f)
        } else {
          // picked a non-singleton
          val fMention =
            if (numNonEmpty == minNumEntities) {
              // non-singleton has a higher probability
              val currNonSingleton = entityList.slice(0, firstEmpty).filter(_.size > 1).foldLeft(0.0)(_ + _.size)
              (1.0 / currNonSingleton)
            } else (1.0 / mentionList.length) // we picked uniformly
          if (e.size == 0) {
            // m cannot be singleton
            val bMention = (1.0 / mentionList.length) // numNonEmpty will be > minNumEntities
            val fEntity = if (numNonEmpty == 1) 1.0 else 1.0 - probRemainFull
            val bEntity = 1.0 / numNonEmpty // next will have numNonEmpty + 1 entities
            math.log((bMention / fMention) * (bEntity / fEntity))
          } else {
            val bMention =
              if (numNonEmpty == minNumEntities) {
                // mention cannot be singleton (m.e.size > 1), and e.size > 0
                // num singletons for the "next" config
                val numNonSingletonMentions = {
                  val currNonSingleton = entityList.slice(0, firstEmpty).filter(_.size > 1).foldLeft(0.0)(_ + _.size)
                  if (e.size == 1 && m.entity.size > 2)
                    currNonSingleton + 1.0 // converted a singleton to non-singletons (e)
                  else if (e.size > 1 && m.entity.size == 2)
                    currNonSingleton - 1.0 // converted a non-singleton to a singleton (m)
                  else
                    currNonSingleton // either non-singleton remained non-singleton, or created/destroyed non-singleton
                }
                (1.0 / numNonSingletonMentions)
              } else (1.0 / mentionList.length)
            //val fEntity = probRemainFull * (1.0 / (numNonEmpty - 1)) //(cancels)
            //val bEntity = probRemainFull * (1.0 / (numNonEmpty - 1)) //(cancels)
            math.log(bMention / fMention)
          }
        }
      // Move it
      m.entityRef.set(e)(difflist)
      return bfRatio
    }

  }

  class Uniform[R <: MentionRecord](model: Model,
                                    var mentionList: Iterable[Mention[R]],
                                    var entityList: Seq[Entity[R]]) extends CorefProposer[R](model) {
    temperature = 0.001

    def setEntities(es: Seq[Entity[R]]) = {
      entityList = es
      mentionList = es.flatMap(_.mentions).toSeq
    }

    def entities = entityList

    def propose(context: Null)(implicit difflist: DiffList): Double = {
      // Pick a random mention
      val m = mentionList.sampleUniformly(cc.factorie.random)
      val entityPos = random.nextInt(entityList.length - 1)
      var e: Entity[R] = if (m.entity.id <= entityPos) entityList(entityPos + 1) else entityList(entityPos)
      // Move it
      m.entityRef.set(e)(difflist)
      // log-Q-ratio shows that forward and backward jumps are equally likely
      return 0.0
    }

  }

  class AffinityBased[R <: MentionRecord](model: Model,
                                          var mentionList: Seq[Mention[R]],
                                          var entityList: Seq[Entity[R]],
                                          val affinityTemplate: PairwiseTemplate[R]) extends CorefProposer[R](model) {

    class QElement(val m1: Mention[R], val m2: Mention[R], var f: Double) {
      override def equals(p1: Any) = p1 match {
        case qe: QElement => m1 == qe.m1 && m2 == qe.m2
        case _ => false
      }
    }

    val probRandom: Double = 0.1
    var numPairs: Double = 0.0
    var norm: Double = 0.0
    var maxValue = Double.NegativeInfinity
    var lastSource: Entity[R] = null
    var lastDest: Entity[R] = null
    var lastMention: Mention[R] = null
    var lastQE: QElement = null

    val queue: Array[Array[QElement]] = Array.fill(mentionList.length, mentionList.length)(null)

    preProcess

    def setEntities(es: Seq[Entity[R]]) = {
      entityList = es
      mentionList = es.flatMap(_.mentions).toSeq
      assert(false, "Need initialization")
    }

    def entities = entityList

    def preProcess = {
      norm = 0.0
      maxValue = Double.NegativeInfinity
      numPairs = 0.0
      //queue.clear
      for (m1: Mention[R] <- mentionList) {
        for (m2: Mention[R] <- mentionList) {
          if (m1.record.id > m2.record.id) {
            var score = affinityTemplate.score(m1.entityRef.value, m2.entityRef.value, m1.value, m2.value)
            score = math.pow(score, 3.0)
            val qe = new QElement(m1, m2, if (m1.entity == m2.entity) -score else score)
            if (math.abs(qe.f) > maxValue) maxValue = math.abs(qe.f)
            norm += math.exp(qe.f)
            queue(m1.record.id)(m2.record.id) = qe
            numPairs += 1
          }
        }
      }
      println("norm: %f, maxValue: %f, pairs: %f".format(norm, maxValue, numPairs))
      lastQE = null
      lastSource = null
      lastDest = null
      lastMention = null
    }

    def sampleExpProportionally(): (Int, Int) = {
      val r = random.nextDouble * norm / math.exp(maxValue)
      var sum = 0.0
      for (m1: Mention[R] <- mentionList) {
        for (m2: Mention[R] <- mentionList) {
          if (m1.record.id > m2.record.id) {
            val e = math.exp(queue(m1.record.id)(m2.record.id).f - maxValue)
            sum += e
            if (sum >= r)
              return (m1.record.id, m2.record.id)
          }
        }
      }
      throw new Error("TraversableExtras sample error: r=" + r + " sum=" + sum)
    }

    // pick a pair from the queue
    def pickPair: (QElement, Boolean) = {
      if (probRandom > random.nextDouble) {
        val i: Int = random.nextInt(mentionList.length)
        val j: Int = random.nextInt(mentionList.length - 1)
        val (m1, m2) = if (j < i) (i, j) else (j + 1, i) // pick a random pair of mentions
        (queue(m1)(m2), true)
      } else {
        val indices = sampleExpProportionally()
        (queue(indices._1)(indices._2), false)
      }
    }

    override def postAcceptanceHook(logAcceptanceProb: Double, d: DiffList): Unit = {
      super.postAcceptanceHook(logAcceptanceProb, d)
      if (d.length != 0) {
        // flip all coreferent with m
        for (m: Mention[R] <- lastDest.value ++ lastSource.value) {
          if (m != lastMention) {
            val (i, j) = if (lastMention.record.id > m.record.id) (lastMention.record.id, m.record.id) else (m.record.id, lastMention.record.id)
            val qe: QElement = queue(i)(j)
            val oldScore = qe.f
            var newScore = -qe.f
            qe.f = newScore
            norm -= math.exp(oldScore)
            norm += math.exp(newScore)
          }
        }
      }
    }

    def propose(context: Null)(implicit difflist: DiffList): Double = {
      val (qe, randomPair) = pickPair
      val (m, otherm) = if (random.nextDouble < 0.5) (qe.m1, qe.m2) else (qe.m2, qe.m1)
      lastMention = m
      lastSource = m.entity
      var forward: Double = 1.0
      var backward: Double = 1.0
      if (m.entity == otherm.entity) {
        // pull them apart by creating a singleton out of m
        lastDest = entityList.find(_.size == 0).get
        forward = (m.entity.size * otherm.entity.size) // pairs which could've resulted in this split
        backward = (lastSource.size - 1) * (lastDest.size + 1) // pairs which can result in this merge
      } else {
        // put them together, move m into otherm
        lastDest = otherm.entity
        forward = (m.entity.size * otherm.entity.size) // pairs which could've resulted in this merge
        backward = (lastDest.size) + (lastSource.size - 1) // pairs which can result in this split
      }
      // val forward: Double = if (randomPair) 1.0 / numPairs else math.exp(qe.f) / norm
      //val backward = (probRandom * numMentions * (1.0 / queue.length)) + ((1 - probRandom) * numMentions * (math.exp(-qe.f) / norm))
      m.entityRef.set(lastDest)(difflist)
      // assume norm doesn't change by much
      // we ignore the actual affinity values for this
      math.log(backward / forward)
    }

  }


  class MoveMany[R <: MentionRecord](model: Model,
                                     var mentionList: Seq[Mention[R]],
                                     var entityList: Seq[Entity[R]]) extends CorefProposer[R](model) {
    temperature = 0.001
    var numFixedMentions: Int = mentionList.length - 1
    val probRemainFull = 0.8
    var nonEmpty: Seq[Entity[R]] = entityList.filter((e: Entity[R]) => e.size > 0)

    def setEntities(es: Seq[Entity[R]]) = {
      entityList = es
      mentionList = es.flatMap(_.mentions).toSeq
      assert(false, "Need initialization")
    }

    def entities = entityList

    override def postAcceptanceHook(logAcceptanceProb: Double, d: DiffList): Unit = {
      // update nonEmpty
      nonEmpty = entityList.filter((e: Entity[R]) => e.size > 0)
    }

    def propose(context: Null)(implicit difflist: DiffList): Double = {
      val numEmpty = entityList.length - nonEmpty.length

      val fixed: Set[Mention[R]] = new HashSet
      fixed ++= mentionList.subseq(numFixedMentions.toDouble / mentionList.length)
      val variable: Set[Mention[R]] = new HashSet
      variable ++= mentionList.filter(!fixed(_))

      var bfRatio: Double = 0.0
      for (m: Mention[R] <- variable) {
        // Pick a random place to move it, either an existing Entity or a newly created one
        var e: Entity[R] = null
        // Pick an existing entity to move it to
        if (m.entityRef.value.size == 1 || random.nextDouble < probRemainFull) {
          val s2 = nonEmpty.filter((e: Entity[R]) => e != m.entityRef.value)
          if (s2.size != 0) e = s2(random.nextInt(s2.size))
        }
        // Pick an empty entity to move it to (create one if it doesn't exist)
        if (e == null) {
          val s2 = entityList.find((e: Entity[R]) => e.size == 0)
          assert(s2.isDefined, "Cannot find empty entity in " + entityList)
          if (s2.isDefined) e = s2.get
        }
        // Move it
        m.entityRef.set(e)(difflist)
      }
      return bfRatio
    }

  }

  class CanopizedSampler[R <: MentionRecord](model: TemplateModel, var mentionList: Seq[Mention[R]] = Seq.empty,
                                             var entityList: Buffer[Entity[R]] = new ArrayBuffer[Entity[R]],
                                             val canopizer: Canopizer[R] = new DefaultCanopizer[R])
                                            (implicit nm1: Manifest[EntityRef[R]], nm2: Manifest[TrueEntityIndex[R]])
        extends CorefProposer[R](model) {
    val objective_ = new Objective(mentionList)
    val emptyEntities = new HashSet[Entity[R]]
    emptyEntities ++= entityList.filter(_.size == 0)
    protected var canopies = new HashMap[String, ArrayBuffer[Mention[R]]]
    initCanopies

    var lastSource: Entity[R] = null
    var lastDest: Entity[R] = null

    def initCanopies = {
      canopies.clear
      mentionList.foreach((m: Mention[R]) => {
        for (cname <- canopizer.canopies(m.record)) {
          canopies.getOrElse(cname, {
            val a = new ArrayBuffer[Mention[R]];
            canopies(cname) = a;
            a
          }) += m
        }
      })
    }

    def entities = entityList

    def setEntities(es: Seq[Entity[R]]) = {
      entityList.clear()
      entityList ++= es
      emptyEntities.clear()
      emptyEntities ++= entityList.filter(_.size == 0)
      mentionList = es.flatMap(_.mentions).toSeq
      lastSource = null
      lastDest = null
      initCanopies
    }

    def nextMention(m: Mention[R] = null): Mention[R] = {
      if (m == null) mentionList.sampleUniformly
      else canopies(m.record.defaultCanopies.sampleUniformly).sampleUniformly
    }

    override def propose(context: Null)(implicit difflist: DiffList): Double = {
      var tries = 100
      do {
        val m1 = nextMention()
        val m2 = nextMention(m1)
        //println("  m1.e.id="+m1.entity.id)
        //println("  m2.e.id="+m2.entity.id)
        lastSource = m2.entity
        if (m1.entity.id == m2.entity.id) {
          // move m2 to empty entity
          val e = if (emptyEntities.size > 0) emptyEntities.head
          else {
            new Entity[R](entityList.size)
          }
          m2.entityRef.set(e)
        } else {
          m2.entityRef.set(m1.entity)
        }
        lastDest = m2.entity
        tries -= 1
        assert(difflist.size != 0, "difflist is empty!: (%d -> %d(%d), %d -> %d(%d)), diff: %s" format(m1.record.id, m1.entity.id, m1.entity.size, m2.record.id, m2.entity.id, m2.entity.size, difflist.toString()))
      } while (tries > 0 && difflist.size == 0)
      0.0
    }

    override def postAcceptanceHook(logAcceptanceProb: Double, d: DiffList) {
      super.postAcceptanceHook(logAcceptanceProb, d)
      if (lastSource.size == 0) {
        assert(!emptyEntities(lastSource))
        emptyEntities += lastSource
      }
      if (lastDest.size == 1) {
        val removed = emptyEntities -= lastDest
        // is lastDest is newly added
        if (removed.isEmpty) entityList += lastDest
      }
    }

    lazy val labeledMentionList = mentionList.filter(_.trueEntityIndex.intValue != -1).toSeq

    override def postProcessHook(context: Null, difflist: DiffList): Unit = {
      super.postProcessHook(context, difflist)
      if ((processCount + 1) % 1000000 == 0) {
        //learningRate *= .9
        println("Accepted: " + numAcceptedMoves + " / " + numProposedMoves)
        println("NumEntites: " + entityList.filter(_.size > 0).size)
        println("Num labeled mentions: " + labeledMentionList.size)
        TimeUtil.snapshot("===== Evaluating =====\n" + Evaluator.evaluate(labeledMentionList))
        println
      }
    }
  }

}