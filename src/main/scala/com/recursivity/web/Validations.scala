package com.recursivity.web

import scalaz._
import Scalaz._

object Validations {
  def validate[A,B](errorValue: A, input: B, validator: (B) => Boolean): ValidationNel[A,B] = {
    if(validator(input)) input.successNel
    else errorValue.failureNel
  }

  private val nums = "1" :: "2" :: "3" :: "4" :: "5" :: "6" :: "7" :: "8" :: "9" :: "0" :: Nil

  private val validChars = "ABCDEFGHIJKLMNOPQRSTUVXYZabcdefghijklmnopqrstuvxyz01234567890!#$%&'*+-/=?^_`{|}~."

  def validPassword(input: String, minLength: Int = 8): Boolean = 
    input.trim.length == input.length && input.toLowerCase != input && input.toUpperCase != input &&
      isStringBetween(input, minLength) && nums.exists(input.contains)
  

  def nonEmptyString(input: String) = isStringBetween(input,1)

  def isEmail(input: String) = {
    val email = input.trim
    val splits = input.split("@")

    !email.startsWith(".") && !email.endsWith(".") && !email.startsWith("@") && !email.endsWith("@") && (splits.length == 2) && 
      (splits(1).reverse.charAt(2) == '.' || splits(1).reverse.charAt(3) == '.')
  }

  def isStringBetween(input: String, min: Int = 0, max: Int = Integer.MAX_VALUE) = (input.trim.length >= min && input.trim.length <= max)
}
