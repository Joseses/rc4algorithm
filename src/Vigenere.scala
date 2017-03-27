import scala.collection.immutable.NumericRange

/**
  * Created by josema on 27/03/17.
  */
object Vigenere {

  val alphabet: NumericRange.Inclusive[Char] = 'A' to 'Z'

  def decryptEncrypt(key: String, text: String): String = {
    var result = ""

    for(i <- 0 until text.length) {
      var a = alphabet.indexOf(text(i))
      var b = alphabet.indexOf(key(i % key.length))
      result += alphabet((a + b) % 26 )
    }

    result
  }
}
