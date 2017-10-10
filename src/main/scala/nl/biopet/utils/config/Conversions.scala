package nl.biopet.utils.config

import java.io.{File, PrintWriter}

import org.yaml.snakeyaml.Yaml
import play.api.libs.json._

import scala.collection.JavaConversions._

object Conversions {

  /**
    * Merge 2 maps, when value is in a map in map1 and map2 the value calls recursively this function
    *
    * @param map1 Prio over map2
    * @param map2 Backup for map1
    * @return merged map
    */
  def mergeMaps(map1: Map[String, Any],
                map2: Map[String, Any],
                resolveConflict: (Any, Any, String) => Any = (m1, _, _) => m1)
    : Map[String, Any] = {
    (for (key <- map1.keySet.++(map2.keySet)) yield {
      if (!map2.contains(key)) key -> map1(key)
      else if (!map1.contains(key)) key -> map2(key)
      else {
        map1(key) match {
          case m1: Map[_, _] =>
            map2(key) match {
              case m2: Map[_, _] =>
                key -> mergeMaps(any2map(m1), any2map(m2), resolveConflict)
              case _ => key -> map1(key)
            }
          case _ => key -> resolveConflict(map1(key), map2(key), key)
        }
      }
    }).toMap
  }

  /** Convert Any to Map[String, Any] */
  def any2map(any: Any): Map[String, Any] = {
    if (any == null) return null
    any match {
      case m: Map[_, _] => m.map(x => x._1.toString -> x._2)
      case m: java.util.LinkedHashMap[_, _] => nestedJavaHashMaptoScalaMap(m)
      case _ =>
        throw new IllegalStateException("Value '" + any + "' is not an Map")
    }
  }

  /** Convert nested java hash map to scala hash map */
  def nestedJavaHashMaptoScalaMap(
      input: java.util.LinkedHashMap[_, _]): Map[String, Any] = {
    input
      .map(value => {
        value._2 match {
          case m: java.util.LinkedHashMap[_, _] =>
            value._1.toString -> nestedJavaHashMaptoScalaMap(m)
          case _ => value._1.toString -> value._2
        }
      })
      .toMap
  }

  lazy val yaml = new Yaml()

  def mapToYaml(map: Map[String, Any]): String =
    yaml.dump(yaml.load(Json.stringify(mapToJson(map))))

  def mapToYamlFile(map: Map[String, Any], outputFile: File): Unit = {
    val writer = new PrintWriter(outputFile)
    writer.println(mapToYaml(map))
    writer.close()
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
      case m: Map[_, _] =>
        mapToJson(m.map(m => m._1.toString -> anyToJson(m._2)))
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
