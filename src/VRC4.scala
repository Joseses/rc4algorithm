import scala.util.Random

/**
  * Created by josema on 28/03/17.
  */
object VRC4 {
  var j: Int = 1

  def vrc4Encrypt(key: String, plainText: String, l: Int): String = {
    if(l < 0) {
      this.j = new Random().nextInt(256)
    } else {
      this.j = l
    }
    val rc4 = new RC4Algorithm("ABCD")
    val crc4 = rc4.encryption("Hello World")
    val c1 = Vigenere.decryptEncrypt(crc4.substring(0,j+1), rc4.kArray.slice(0,j+1).mkString(""), encrypt = true)
    //val c1 = Vigenere.decryptEncrypt(rc4.kArray.slice(0,j).mkString(""), crc4.substring(0,j), encrypt = true)
    val c2 = Vigenere.decryptEncrypt(crc4.substring(j,crc4.length), rc4.kArray.slice(j,255).mkString(""), encrypt = true)
    //val c2 = Vigenere.decryptEncrypt(rc4.kArray.slice(j,256).mkString(""), crc4.substring(j,crc4.length), encrypt = true)

    val cvigenere = c1 + c2 + j

    cvigenere
  }

  def main(args: Array[String]): Unit = {
    println(VRC4.vrc4Encrypt("ABCD", "Hello World", 1))
    println(Vigenere.decryptEncrypt("AB", "AB", encrypt = true))
  }
}
