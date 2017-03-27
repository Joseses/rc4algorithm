/**
  * Creado por José Manuel Jiménez Ávila - 22/03/17.
  * josema
  */
class RC4Algorithm(key:String) {

  var sArray = new Array[Int](256)
  var tArray = new Array[Int](256)
  var kArray: Array[Char] = key.toCharArray

  initialization()
  sPermutation()

  def encryption(plaintext: String): String = {
    var i = 0; var j = 0; var k = 0; var t = 0; var counter = 0
    var ciphertext = ""
    while ( counter < plaintext.length ) {
      i = (i + 1) & 0xFF
      j = (j + sArray(i)) & 0xFF
      val temp = sArray(i)
      sArray(i) = sArray(j)
      sArray(j) = temp
      t = (sArray(i) + sArray(j)) & 0xFF
      k = sArray(t)


      ciphertext += "00" + Integer.toString(plaintext(counter).toByte ^ k, 16) takeRight 2

      counter += 1
    }

    ciphertext
  }

  def decryption(ciphertext: String): String =
  {
    var i = 0; var j = 0; var k = 0; var t = 0; var counter = 0
    var plaintext = ""
    for(counter <- 0 to ciphertext.length by 2)
      {
        i = (i + 1) & 0xFF
        j = (j + sArray(i)) & 0xFF
        val temp = sArray(i)
        sArray(i) = sArray(j)
        sArray(j) = temp
        t = (sArray(i) + sArray(j)) & 0xFF
        k = sArray(t)

        if (counter !=ciphertext.length ) {
          var unSubString = ciphertext.substring(counter, counter + 2)
          var number = Integer.parseInt(unSubString, 16)

          plaintext += "00"+ Integer.toString(number.toByte^k,16) takeRight 2

        }

      }
    plaintext

  }


  def initialization(): Unit = {
    for(i <- sArray.indices){
      sArray(i) = i
      tArray(i) = kArray(i % key.length)
    }
  }

  def sPermutation(): Unit = {
    var j = 0
    for(i <- 0 until 256) {
      j = (j + sArray(i) + tArray(i)) & 0xFF
      swap(sArray,i,j)
    }
  }

  def swap[T](array:Array[T], posA: Int, posB: Int): Unit = {
    val tempValue = array(posA)
    array(posA) = array(posB)
    array(posB) = tempValue
  }
}

object default {
  def main(args: Array[String]): Unit = {
    val rc4 = new RC4Algorithm("ABCD")
    //println(rc4.encryption("Hello World"))
    print(rc4.decryption("700196db60f45629a54046"))
  }
}
