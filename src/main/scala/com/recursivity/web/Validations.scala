package com.recursivity.web

import scalaz._
import Scalaz._

object Validations {
  def validate[A,B](errorValue: A, input: B, validator: (B) => Boolean): ValidationNel[A,B] = {
    if(validator(input)) input.successNel
    else errorValue.failureNel
  }

  private val nums = "1" :: "2" :: "3" :: "4" :: "5" :: "6" :: "7" :: "8" :: "9" :: "0" :: Nil

  def validPassword(input: String, minLength: Int = 8): Boolean = 
    input.trim.length == input.length && input.toLowerCase != input && input.toUpperCase != input &&
      isStringBetween(input, minLength) && nums.exists(input.contains)
  

  def nonEmptyString(input: String) = isStringBetween(input,1)

  def isEmail(input: String) = {
    val email = "^[_A-Za-z0-9-\\+]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})$".r
    (email.pattern.matcher(input).matches())
  }

  def isStringBetween(input: String, min: Int = 0, max: Int = Integer.MAX_VALUE) = (input.trim.length >= min && input.trim.length <= max)
}
