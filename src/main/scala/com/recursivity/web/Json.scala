package com.recursivity.web

import unfiltered.response._
import net.liftweb.json.{MappingException, TypeInfo, Serializer, Formats}
import net.liftweb.json.JsonAST._
import net.liftweb.json.Extraction._
import net.liftweb.json.Printer._
import net.liftweb.json.JsonParser._
import scalaz._
import unfiltered.response.ResponseString
import org.joda.time.DateTime

object Json {
  val defaultFormats = (net.liftweb.json.DefaultFormats + BigDecimalSerializer + DateTimeSerializer)

  def toJson[A](value: A)(implicit jsonFormats: Formats = defaultFormats): String = compact(render(decompose(value)))

  def fromJson[A](jsonBody: String)(implicit jsonFormats: Formats = defaultFormats, manifest: Manifest[A]): A = jsonValue(jsonBody).extract[A]

  def jsonValue(jsonBody: String)(implicit jsonFormats: Formats = defaultFormats): JValue = parse(jsonBody)

}

object JsonView{
  import Json._

  implicit def unitToResponse(unit: Unit) = NoContent

  implicit def productToResponse[A](model: A)(implicit jsonFormats: Formats = defaultFormats) = Ok ~> JsonContent ~> ResponseString(toJson(model))

  implicit def validationResultToResponse[A,B](validationResult: ValidationNel[A,B])(implicit jsonFormats: Formats = defaultFormats) = {
    validationResult.fold(fail => {
      BadRequest ~> JsonContent ~> ResponseString(compact(render(JArray(fail.list map decompose))))
    },success => productToResponse(success))
  }

}

object BigDecimalSerializer extends Serializer[BigDecimal]{
  private val BigDecimalClass = classOf[BigDecimal]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), BigDecimal] = {
    case (TypeInfo(BigDecimalClass, _), json) => json match {
      case JDouble(d) => BigDecimal(d)
      case x => throw new MappingException("Can't convert " + x + " to BigDecimal")
    }
  }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case x: BigDecimal => JDouble(x.toDouble)
  }
}

object DateTimeSerializer extends Serializer[DateTime]{
  import java.text.SimpleDateFormat
  private val DateTimeClass = classOf[DateTime]
  private def df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS")

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), DateTime] = {
    case (TypeInfo(DateTimeClass, _), json) => json match {
      case JString(d) => new DateTime(df.parse(d))
      case x => throw new MappingException("Can't convert " + x + " to DateTime")
    }
  }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case x: DateTime => JString(df.format(x.toDate()))
  }
}
