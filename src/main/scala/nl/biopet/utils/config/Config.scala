package nl.biopet.utils.config

import java.io.File

import com.codahale.jerkson.Json._
import nl.biopet.utils.Logging
import org.yaml.snakeyaml.Yaml
import play.api.libs.json._

import scala.io.Source

case class Config(rootJson: JsObject) {

  def samples: Map[String, JsObject] = {
    (rootJson \ "samples").validate[JsObject].getOrElse(Json.obj())
      .fields.map(x => x._1 -> x._2.validate[JsObject].getOrElse(Json.obj())).toMap
  }

  def libraries(sample: String): Map[String, JsObject] = {
    (samples(sample) \ "libraries").validate[JsObject].getOrElse(Json.obj())
      .fields.map(x => x._1 -> x._2.validate[JsObject].getOrElse(Json.obj())).toMap
  }

  def readgroups(sample: String, library: String): Map[String, JsObject] = {
    (libraries(sample)(library) \ "readgroups").validate[JsObject].getOrElse(Json.obj())
      .fields.map(x => x._1 -> x._2.validate[JsObject].getOrElse(Json.obj())).toMap
  }

}

object Config extends Logging {
  def fromFile(file: File): Config = {
    val value = if (file.getName.endsWith(".json"))
      Json.parse(Source.fromFile(file).mkString)
    else loadYaml(file)

    value match {
      case o: JsObject => Config(o)
      case _ => throw new IllegalStateException("Should be a object")
    }
  }

  def loadYaml(file: File): JsValue = {
    loadYaml(Source.fromFile(file).mkString)
  }


  def loadYaml(content: String): JsValue = {
    val yaml = (new Yaml).load(content)
    Json.parse(generate(yaml))
  }
}
