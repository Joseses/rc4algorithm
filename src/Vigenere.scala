import scala.collection.immutable.NumericRange
import math.abs

/**
  * Created by josema on 27/03/17.
  */
object Vigenere {

  val alphabet: NumericRange.Inclusive[Char] = 'A' to 'Z'

  def decryptEncrypt(key: String, text: String, encrypt: Boolean): String = {
    var result = ""

    for(i <- 0 until text.length) {
      val a = alphabet.indexOf(text(i))
      val b = alphabet.indexOf(key(i % key.length))
      if(encrypt) {
        result += alphabet((a + b) % 26 )
      } else {
        result += alphabet(abs((a - b) % 26))
      }
    }

    result
  }
}
