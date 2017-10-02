package nl.biopet.utils.config

import java.io.{File, PrintWriter}

import org.scalatest.Matchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import play.api.libs.json.{JsObject, JsString, Json}

class ConfigTest extends TestNGSuite with Matchers {
  @Test
  def testYaml(): Unit = {
    val yamlValue =
      """
       |key: value
       |nested:
       |  key: value2
       |""".stripMargin
    val value = Config.loadYaml(yamlValue)
    (value \ "key").as[String] shouldBe "value"
    (value \ "nested" \ "key").as[String] shouldBe "value2"
    (value \ "nested").as[Map[String, String]] shouldBe Map("key" -> "value2")

    val tempFile = File.createTempFile("test.", ".yaml")
    tempFile.deleteOnExit()
    val writer = new PrintWriter(tempFile)
    writer.println(yamlValue)
    writer.close()

    Config.fromFile(tempFile).rootJson shouldBe value
  }

  val yamlSamples: String =
    """
      |samples:
      |  sample1:
      |    R1: test1
      |    libraries:
      |      lib1:
      |        R1: test2
      |        readgroups:
      |          rg1:
      |            R1: test3
      |  sample2:
      |    R1: test4
      |""".stripMargin

  @Test
  def testSamples(): Unit = {
    val value = Config.loadYaml(yamlSamples)
    val config = Config(value.validate[JsObject].getOrElse(Json.obj()))
    val samples = config.samples
    samples.keySet shouldBe Set("sample1", "sample2")
    (samples("sample1") \ "R1").get shouldBe JsString("test1")
    (samples("sample2") \ "R1").get shouldBe JsString("test4")
  }

  @Test
  def testLibraries(): Unit = {
    val value = Config.loadYaml(yamlSamples)
    val config = Config(value.validate[JsObject].getOrElse(Json.obj()))
    val libraries1 = config.libraries("sample1")
    val libraries2 = config.libraries("sample2")
    libraries1.keySet shouldBe Set("lib1")
    libraries2.keySet shouldBe Set()
    (libraries1("lib1") \ "R1").get shouldBe JsString("test2")
  }

  @Test
  def tesReadgroups(): Unit = {
    val value = Config.loadYaml(yamlSamples)
    val config = Config(value.validate[JsObject].getOrElse(Json.obj()))
    val readgroups = config.readgroups("sample1", "lib1")
    readgroups.keySet shouldBe Set("rg1")
    (readgroups("rg1") \ "R1").get shouldBe JsString("test3")
  }
}
