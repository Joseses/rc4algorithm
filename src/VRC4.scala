import scala.util.Random

/**
  * 148100, ,
  */
object VRC4 {
  var j: Int = 1

  def vrc4Encrypt(key: String, plainText: String, l: Int): String = {
    if(l < 0) {
      this.j = new Random().nextInt(256)
    } else {
      this.j = l
    }
    var rc4 = new RC4Algorithm(key)
    val crc4 = hexascii(rc4.encryption(plainText))

    val c1 = Vigenere.decryptEncrypt(rc4.kArray.slice(0,j).mkString(""), crc4.substring(0,j), encrypt = true)
    val c2 = Vigenere.decryptEncrypt(rc4.kArray.slice(j,256).mkString(""), crc4.substring(j,crc4.length), encrypt = true)

    val cvigenere = c1 + c2 + j

    cvigenere
  }

  def vrc4Decrypt(key: String, cipherText: String): String = {
    this.j = cipherText.last.asDigit
    val newCT = cipherText.substring(0,cipherText.length-1)
    var rc4 = new RC4Algorithm(key)

    val c1 = Vigenere.decryptEncrypt(rc4.kArray.slice(0,j).mkString(""), newCT.substring(0,j), encrypt = false)
    val c2 = Vigenere.decryptEncrypt(rc4.kArray.slice(j,256).mkString(""), newCT.substring(j,newCT.length), encrypt = false)

    var charArray = (c1+c2).toCharArray
    val toDecrypt = charArray.map("00" + _.toHexString takeRight 2).mkString("")
    rc4 = new RC4Algorithm(key)
    val crc4 = rc4.decryption(toDecrypt)

    crc4
  }

  def main(args: Array[String]): Unit = {
    val cipherText = VRC4.vrc4Encrypt("ABCD", "Hello World", 1)
    println("Cipher text: " + cipherText + ",  length: " + cipherText.length)
    val plainTextHex = this.vrc4Decrypt("ABCD", cipherText)
    println("Plain text: " + hexascii(plainTextHex))

  }

  def hexascii (hex: String): String = {
    var another = ""
    val hexArray = hex.split("(?<=\\G..)")
    for(i <- hexArray) {
      another += Integer.parseInt(i, 16).toChar
    }
    another
  }
}
