package org.sameersingh.utils.coref

import org.junit._

/**
 * @author sameer
 */

@Test
class EntityMapsTest {

  @Test
  def testKeys(): Unit = {
    val e = new GenericEntityMap[String] with Key[String, String]
    e.addMention("m0", "e0")
    e.addMention("m1", "e0")
    e.addMention("m2", "e0")
    e.addMention("m4", "e1")
    println(e)
  }

}