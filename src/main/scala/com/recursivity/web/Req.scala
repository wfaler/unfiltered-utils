package com.recursivity.web

import org.joda.time.DateTime

/**
 * Note that for the Seq based conversions, an empty list will always be returned if any one element fails conversion
 */
object Req {
  import Conversions._

  def int(name: String)(implicit params: Map[String,Seq[String]]): Option[Int] = toOpt(toInt(_), string(name))

  def long(name: String)(implicit params: Map[String,Seq[String]]): Option[Long] = toOpt(toLong(_), string(name))

  def double(name: String)(implicit params: Map[String,Seq[String]]): Option[Double] = toOpt(toDouble(_), string(name))

  def decimal(name: String)(implicit params: Map[String,Seq[String]]): Option[BigDecimal] = toOpt(toDecimal(_), string(name))

  def bool(name: String)(implicit params: Map[String,Seq[String]]): Option[Boolean] = {
    string(name).collect({
      case "0" => false
      case "1" => true
      case "true" => true
      case "false" => false
    })
  }

  def date(name: String)(implicit params: Map[String,Seq[String]]): Option[DateTime] = toOpt(toDateTime(_), string(name))

  def string(name: String)(implicit params: Map[String,Seq[String]]): Option[String] = if(params(name).length == 1) params(name).headOption else None

  // plurals

  def ints(name: String)(implicit params: Map[String,Seq[String]]): Seq[Int] = seqOrNil(name, toInt)

  def longs(name: String)(implicit params: Map[String,Seq[String]]): Seq[Long] = seqOrNil(name, toLong)

  def doubles(name: String)(implicit params: Map[String,Seq[String]]): Seq[Double] = seqOrNil(name, toDouble)

  def decimals(name: String)(implicit params: Map[String,Seq[String]]): Seq[BigDecimal] = seqOrNil(name, toDecimal)

  def bools(name: String)(implicit params: Map[String,Seq[String]]): Seq[Boolean] = seqOrNil(name, (b) => b match{
    case "0" => false
    case "1" => true
    case "true" => true
    case "false" => false
  })

  def dates(name: String)(implicit params: Map[String,Seq[String]]): Seq[DateTime] = seqOrNil(name, toDateTime)

  def strings(name: String)(implicit params: Map[String,Seq[String]]): Seq[String] = params(name)

  def seqOrNil[A](name: String, f: (String) => A)(implicit params: Map[String,Seq[String]]): Seq[A] = {
    val seq = params(name) map{v => toOpt(f,v)}
    if(seq forall(_.isDefined)) seq flatMap {_.toSeq} else Nil
  }

}

object Conversions{
  def toInt(s: String) = s.toInt
  def toLong(s: String) = s.toLong
  def toDouble(s: String) = s.toDouble
  def toDecimal(s: String) = BigDecimal(s)
  def toDateTime(s: String): DateTime = null

  def toOpt[A](f: (String) => A, a: Option[String]): Option[A] = a flatMap{v => try Some(f(v)) catch{ case e: Exception => None}}
  def toOpt[A](f: (String) => A, a: String): Option[A] = try Some(f(a)) catch{ case e: Exception => None}
}
