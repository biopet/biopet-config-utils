package nl.biopet.utils.config

import java.io.File

import com.codahale.jerkson.Json._
import nl.biopet.utils.Logging
import org.yaml.snakeyaml.Yaml
import play.api.libs.json._

import scala.io.Source

case class Config(rootJson: JsObject) {

  def samples: Map[String, JsObject] = {
    (rootJson \ "samples")
      .validate[JsObject]
      .getOrElse(Json.obj())
      .fields
      .map(x => x._1 -> x._2.validate[JsObject].getOrElse(Json.obj()))
      .toMap
  }

  def libraries(sample: String): Map[String, JsObject] = {
    (samples(sample) \ "libraries")
      .validate[JsObject]
      .getOrElse(Json.obj())
      .fields
      .map(x => x._1 -> x._2.validate[JsObject].getOrElse(Json.obj()))
      .toMap
  }

  def readgroups(sample: String, library: String): Map[String, JsObject] = {
    (libraries(sample)(library) \ "readgroups")
      .validate[JsObject]
      .getOrElse(Json.obj())
      .fields
      .map(x => x._1 -> x._2.validate[JsObject].getOrElse(Json.obj()))
      .toMap
  }

}

object Config extends Logging {
  def fromFile(file: File): Config = {
    val value =
      if (file.getName.endsWith(".json"))
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

  def fromMap(map: Map[String, Any]): Config = {
    Config(mapToJson(map))
  }

  /** Convert native scala map to json */
  def mapToJson(map: Map[String, Any]): JsObject = {
    JsObject.apply(map.map(x => x._1 -> anyToJson(x._2)))
  }

  /** Convert native scala value to json, fall back on .toString if type is not a native scala value */
  def anyToJson(any: Any): JsValue = {
    any match {
      case j: JsValue => j
      case None => JsNull
      case Some(x) => anyToJson(x)
      case m: Map[_, _] => mapToJson(m.map(m => m._1.toString -> anyToJson(m._2)))
      case l: List[_] => JsArray(l.map(anyToJson))
      case l: Array[_] => JsArray(l.map(anyToJson))
      case b: Boolean => JsBoolean(b)
      case n: Int => JsNumber(n)
      case n: Double => JsNumber(n)
      case n: Long => JsNumber(n)
      case n: Short => JsNumber(n.toInt)
      case n: Float => JsNumber(n.toDouble)
      case n: Byte => JsNumber(n.toInt)
      case null => JsNull
      case _ => JsString(any.toString)
    }
  }

}
