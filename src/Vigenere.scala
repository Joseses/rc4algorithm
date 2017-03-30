import scala.collection.immutable.NumericRange

/**
  * 148100, ,
  */
object Vigenere {

  val alphabet: NumericRange.Inclusive[Char] = '\u0000' to '\u0200'

  def decryptEncrypt(key: String, text: String, encrypt: Boolean): String = {
    var result = ""
    //println("Trying with key: " + key + "(" + key.length + "), text: " + text + "(" + text.length + ")")
    for(i <- 0 until text.length) {
      val a = alphabet.indexOf(text(i))
      val b = alphabet.indexOf(key(i % key.length))
      if((a ^ b) < 0) {
        println("Something went wrong :(")
      }else {
        if (encrypt) {
          result += alphabet((a + b) % alphabet.length)
        } else {
          var modulo = (a - b) % alphabet.length
          if (modulo < 0) {
            modulo += alphabet.length
          }
          result += alphabet(modulo)
        }
      }
    }

    result
  }

  def main(args: Array[String]): Unit = {
    val cipherText = Vigenere.decryptEncrypt("LEMON","ATTACKATDAWN", encrypt = true)
    println(cipherText)
    println(Vigenere.decryptEncrypt("LEMON",cipherText, encrypt = false))
  }
}
