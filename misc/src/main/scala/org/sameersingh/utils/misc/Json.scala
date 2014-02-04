package org.sameersingh.utils.misc

import java.lang.reflect.{Type, ParameterizedType}
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.core.`type`.TypeReference
import java.io.File
import scala.io.Source

/**
 * @author sameer
 * @since 2/4/14.
 */
object Json {
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  def generate(value: Any, f: File): Unit = {
    mapper.writeValue(f, value)
  }

  def generate(value: Any): String = {
    import java.io.StringWriter
    val writer = new StringWriter()
    mapper.writeValue(writer, value)
    writer.toString
  }

  def parse[T: Manifest](value: String) : T =
    mapper.readValue(value, typeReference[T])

  def parse[T: Manifest](f: File) : T = {
    val source = Source.fromFile(f)
    val t : T = parse[T](source.getLines().mkString("\n"))
    source.close()
    t
  }

  private [this] def typeReference[T: Manifest] = new TypeReference[T] {
    override def getType = typeFromManifest(manifest[T])
  }

  private [this] def typeFromManifest(m: Manifest[_]): Type = {
    if (m.typeArguments.isEmpty) { m.erasure }
    else new ParameterizedType {
      def getRawType = m.erasure
      def getActualTypeArguments = m.typeArguments.map(typeFromManifest).toArray
      def getOwnerType = null
    }
  }
}