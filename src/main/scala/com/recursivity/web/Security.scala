package com.recursivity.web

import java.security.SecureRandom
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec

import unfiltered.request._
import unfiltered.response._

import unfiltered.Cookie

object PBKDF2Password{
  /**
  * Assumes that the salt unique for each user is stored together with the password derived key
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
  def accessAllowed[B](sessionId: String)(block: => ResponseFunction[B]): ResponseFunction[B] = if(isValidSession(sessionId)) block else Unauthorized

  def isValidSession(sessionId: String): Boolean

  def invalidateSession(sessionId: String)
}

trait SessionIntent extends AccessControl{
  import Req._

  //type UnfilteredIntent[A] = PartialFunction[HttpRequest[A], ResponseFunction[A]]

  def cookieName: String

  val userParam: String = "username"

  val passParam: String = "password"

  def signIn(username: String, password: String): Option[String]
  
  val sessionIntent = unfiltered.filter.Planify {
    case req @ GET(Path(Seg("api" :: "session" :: Nil))) & Cookies(cookies) => 
      cookies(cookieName) map{c => if(isValidSession(c.value)) NoContent else Unauthorized} getOrElse Unauthorized
    case req @ POST(Path(Seg("api" :: "session" :: Nil))) & Params(params) => {
      implicit val parameters = params
      signIn(string(userParam) getOrElse "", string(passParam) getOrElse "") map
        {session => SetCookies(Cookie(cookieName, session, path = Some("/"))) ~> Ok} getOrElse Unauthorized
    }
    case req @ DELETE(Path(Seg("api" :: "session" :: Nil))) => forget
    case req @ GET(Path(Seg("api" :: "session" :: "forget" :: Nil))) => forget
    case _ => Pass  
  }

  private def forget = SetCookies(Cookie(cookieName, "", path = Some("/"), maxAge = Some(0))) ~> Ok
}
