package com.recursivity.web

import org.specs2.mutable._
import com.recursivity.web.Html.View

import scalaz._
import Scalaz._
import org.joda.time.DateTime 
import org.joda.time.DateTimeZone


class WebSpec extends Specification{
  val dateHolder = new DateHolder(new DateTime)

  "The Html" should{
    "be able to render a template" in{
       Html.renderHtml(View("/child.mustache", Map("name" -> "Wille"))) must_== "Hello Wille"
    }
    "be able to render a template within a parent hierarchically" in{
      Html.renderHtml(View("/child.mustache", Map("name" -> "Wille"), Some((View("/parent.mustache", Map.empty), "child")))) must_== "Parent Hello Wille"
    }
  }

  "Json" should{
    "be able to create an object from a json string" in{
      Json.fromJson[Foo]("""{"bar": "drink", "age": 30.1}""") must_==(Foo("drink",BigDecimal(30.1)))
    }
    "be able to create json string from an object" in{
      Json.toJson(Foo("drink",BigDecimal(30.1))) must_==("""{"bar":"drink","age":30.1}""")
    }
    "be able to create json string from an object with a dateTime" in{
      Json.fromJson[DateHolder](Json.toJson(dateHolder)) must be_==(dateHolder)
    }
  }

  "Security" should{
    "be able to generate and validate a password" in{
      val salt = PBKDF2Password.generateSalt()
      val pass = PBKDF2Password.encryptedPassword("foobar", salt)
      PBKDF2Password.authenticate("foobar",pass,salt) must beTrue
    }
    "be able to invalidate an invalid password" in{
      val salt = PBKDF2Password.generateSalt()
      val pass = PBKDF2Password.encryptedPassword("foobar", PBKDF2Password.generateSalt())
      PBKDF2Password.authenticate("foobar",pass,salt) must beFalse
    }
  }
  import Validations._

  "Validations" should{
    "return a list of errors if invalid" in{
      ((validate("error1", "", nonEmptyString) |@| validate("error2", "", nonEmptyString) |@| validate("error3", "foo", nonEmptyString)){
        (in1,in2,in3) => "correct!"
	    }).fold(f => f.toList, s => s :: Nil) must_==("error1" :: "error2" :: Nil)
    }

    "build applicatively if invalid" in{
      ((validate("error1", "f", nonEmptyString) |@| validate("error2", "a", nonEmptyString) |@| validate("error3", "foo", nonEmptyString)){
        (in1,in2,in3) => "correct!"
      }).fold(f => f.toList, s => s :: Nil) must_==("correct!" :: Nil)      
    }
    "validate e-mails correctly" in {
      isEmail("wf@foobar.com") must beTrue
    }
    "validate e-mails with subdomains correctly" in {
      isEmail("wf.ab@sub.foobar.com") must beTrue
    }
    "invalidate e-mails correctly" in {
      isEmail("wf@foobar.c") must beFalse
    }
  }

  implicit def valToSeq[A](a: A): Seq[A] = Seq(a)

  implicit val req = Map[String,Seq[String]]("check" -> "on", "bool" -> "true","int" -> "1", 
    "invalidNum" -> "foo", "invalidOpt" -> Seq("1","2")).withDefaultValue(Nil) 

  import Req._
  import Conversions._

  "Request mapping" should{
     "deal with ints correctly" in{
       int("int") must be_==(Some(1))
       int("invalidNum") must be_==(None)
       ints("int") must be_==(Seq(1))
       ints("invalidOpt") must be_==(Seq(1,2))
       int("invalidOpt") must beNone
     }
     "deal with longs correctly" in{
       long("int") must be_==(Some(1l))
       long("invalidNum") must be_==(None)
       longs("int") must be_==(Seq(1l))
       longs("invalidOpt") must be_==(Seq(1l,2l))
       long("invalidOpt") must beNone
     }
     "deal with doubles correctly" in{
       double("int") must be_==(Some(1d))
       double("invalidNum") must be_==(None)
       doubles("int") must be_==(Seq(1d))
       doubles("invalidOpt") must be_==(Seq(1d,2d))
       double("invalidOpt") must beNone
     }
     "deal with decimals correctly" in{
       decimal("int") must be_==(Some(BigDecimal(1)))
       decimal("invalidNum") must be_==(None)
       decimals("int") must be_==(Seq(BigDecimal(1)))
       decimals("invalidOpt") must be_==(Seq(BigDecimal(1),BigDecimal(2)))
       decimal("invalidOpt") must beNone
     }

     "deal with booleans correctly" in{
       bool("int") must be_==(true)
       bool("bool") must be_==(true)
       bool("check") must be_==(true)
       bool("invalidNum") must be_==(false)
       bool("nonexistent") must be_==(false)
       bools("invalidOpt") must be_==(true :: false :: Nil)
     }

     "deal with dates correctly" in{
       toDateTime("2001-07-04T12:08:56.235").getYear() must be_==(2001)
       toDateTime("2001-07-04 12:08:56.235").getYear() must be_==(2001)
       toDateTime("2001-07-04T12:08:56").getYear() must be_==(2001)
       toDateTime("2001-07-04 12:08").getYear() must be_==(2001)
       toDateTime("2001-07-04").getYear() must be_==(2001)
     }
  }

}

case class Foo(bar: String, age: BigDecimal)

case class DateHolder(date: DateTime)







