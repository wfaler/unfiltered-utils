package com.recursivity.web

import scalaz._
import Scalaz._
import unfiltered.request.Params

class Validation {
  def validate[A,B](errorValue: A, input: B, validator: (B) => Boolean): ValidationNel[A,B] = {
    if(validator(input)) input.successNel
    else errorValue.failureNel
  }

  def nonEmptyString(input: String) = isStringBetween(1, Integer.MAX_VALUE, input)

  def isEmail(input: String) = {
    val email = "^[_A-Za-z0-9-\\+]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})$".r
    (email.pattern.matcher(input).matches())
  }

  def isStringBetween(min: Int, max: Int, input: String) = (input.trim.length >= min && input.trim.length <= max)
}
