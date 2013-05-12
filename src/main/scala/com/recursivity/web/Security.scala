package com.recursivity.web

import java.security.SecureRandom
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec

import unfiltered.request._
import unfiltered.response._

import unfiltered.Cookie

object PBKDF2Password{
  /** Assumes that the salt unique for each user is stored together with the password derived key
  */
  def authenticate(attemptedPassword: String, encryptedPass: Array[Byte], salt: Array[Byte]): Boolean = {
    java.util.Arrays.equals(encryptedPass, encryptedPassword(attemptedPassword, salt))
  }

  def encryptedPassword(password: String, salt: Array[Byte], iterations: Int = 20000, keyLength: Int = 160): Array[Byte] = {
    val spec = new PBEKeySpec(password.toCharArray(), salt, iterations, keyLength)
    val keyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    keyFactory.generateSecret(spec).getEncoded()
  }

  def generateSalt(): Array[Byte] = {
    val random = SecureRandom.getInstance("SHA1PRNG")
    val salt = new Array[Byte](8)
    random.nextBytes(salt)
    salt
  }

}

trait AccessControl{
  def accessAllowed[A,B](sessionId: A)(block: => ResponseFunction[B]): ResponseFunction[B] = if(isValidSession(sessionId)) block else Unauthorized

  def isValidSession[A](sessionId: A): Boolean

  def invalidateSession[A](sessionId: A)
}

trait SignInIntent[A] extends AccessControl{
  import Req._

  type UnfilteredIntent[A] = PartialFunction[HttpRequest[A], ResponseFunction[A]]

  def cookieName: String

  def userParam: String

  def passParam: String

  def signIn[A](username: String, password: String): Option[A]
  
  def sessionIntent: UnfilteredIntent[A] = {
    case req @ GET(Path(Seg("api" :: "session" :: Nil))) & Cookies(cookies) => 
      cookies(cookieName) map{c => if(isValidSession(c.value)) NoContent else Unauthorized} getOrElse Unauthorized
    case req @ POST(Path(Seg("api" :: "session" :: Nil))) & Params(params) => {
      implicit val parameters = params
      signIn(string(userParam) getOrElse "", string(passParam) getOrElse "") map
        {session => SetCookies(Cookie(cookieName, session.toString, path = Some("/"))) ~> NoContent} getOrElse Unauthorized
    }
    case req @ DELETE(Path(Seg("api" :: "session" :: Nil))) => forget
    case req @ GET(Path(Seg("api" :: "session" :: "forget" :: Nil))) => forget
    case _ => Pass  
  }

  private def forget = SetCookies(Cookie(cookieName, "")) ~> NoContent
}
