package com.recursivity.web

import java.security.SecureRandom
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec


object PBKDF2Password{

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