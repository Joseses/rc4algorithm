/**
  * 148100, ,
  */
class RC4Algorithm(key:Array[Int], arr: Array[Int]) {

  var sArray: Array[Int] = arr
  var tArray = new Array[Int](256)
  var kArray: Array[Int] = key

  def this(key: String) = this(key.map(_.toInt).toArray, new Array[Int](256))

  initialization()
  sPermutation()

  def encryption(plaintext: String): String = {
    var i = 0; var j = 0; var k = 0; var t = 0; var counter = 0
    var ciphertext = ""
    while ( counter < plaintext.length ) {
      i = (i + 1) & (sArray.length - 1)
      j = (j + sArray(i)) & (sArray.length - 1)
      val temp = sArray(i)
      sArray(i) = sArray(j)
      sArray(j) = temp
      t = (sArray(i) + sArray(j)) & (sArray.length - 1)
      k = sArray(t)

      ciphertext += "00" + Integer.toString(plaintext(counter).toInt ^ k, 16) takeRight 2

      counter += 1
    }
    ciphertext
  }

  def decryption(ciphertext: String): String = {
    var i = 0; var j = 0; var k = 0; var t = 0; var counter = 0
    var plaintext = ""

    var charArray : Array[String] = ciphertext.split("(?<=\\G..)")

    for(index <- charArray.indices) {
      i = (i + 1) & (sArray.length - 1)
      j = (j + sArray(i)) & (sArray.length - 1)
      val temp = sArray(i)
      sArray(i) = sArray(j)
      sArray(j) = temp
      t = (sArray(i) + sArray(j)) & (sArray.length - 1)
      k = sArray(t)

      var number = Integer.parseInt(charArray(index), 16)

      plaintext += "00"+ Integer.toString(number^k,16) takeRight 2

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
    for(i <- 0 until (sArray.length - 1) ) {
      j = (j + sArray(i) + tArray(i)) & (sArray.length - 1)
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
    val rc4D = new RC4Algorithm("ABCD")
    println(rc4.encryption("Hello World"))
    println(rc4D.decryption("700196db60f45629a54046"))

    val rc42 = new RC4Algorithm(Array(2, 5), Array(0, 1, 2, 3))
    val rc42D = new RC4Algorithm(Array(2, 5), Array(0, 1, 2, 3))
    var encrypted = rc42.encryption("hi")
    println(encrypted)
    println(rc42D.decryption(encrypted))
    //Beto es jo..ven
  }
}
