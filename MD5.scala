import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.IntBuffer

object MD5S extends App
{
  val INIT_A: Int = 0x67452301
  val INIT_B: Int = 0xEFCDAB89
  val INIT_C: Int = 0x98BADCFE
  val INIT_D: Int = 0x10325476
 
  val SHIFT_AMTS: Array[Int] = Array(
    7, 12, 17, 22,
    5,  9, 14, 20,
    4, 11, 16, 23,
    6, 10, 15, 21
  )
 
  val TABLE_T: Array[Int] = new Array[Int](64)

  for (i <- 0 until 64) {
    TABLE_T(i) = ((1L << 32) * Math.abs(Math.sin(i + 1))).toLong.toInt
  }
 
  def computeMD5(message: Array[Byte]): Array[Byte] = {
    val padded: ByteBuffer = ByteBuffer.allocate((((message.length + 8) / 64) + 1) * 64).order(ByteOrder.LITTLE_ENDIAN)
    padded.put(message)
    padded.put(0x80.toByte)
    val messageLenBits: Long = message.length * 8
    padded.putLong(padded.capacity() - 8, messageLenBits)
 
    padded.rewind()
 
    var a: Int = INIT_A
    var b: Int = INIT_B
    var c: Int = INIT_C
    var d: Int = INIT_D
    while (padded.hasRemaining()) {
      // obtain a slice of the buffer from the current position,
      // and view it as an array of 32-bit ints
      val chunk: IntBuffer = padded.slice().order(ByteOrder.LITTLE_ENDIAN).asIntBuffer()
      val originalA: Int = a
      val originalB: Int = b
      val originalC: Int = c
      val originalD: Int = d
      for (j <- 0 until 64)
      {
        val div16: Int = j >>> 4
        var f: Int = 0
        var bufferIndex: Int = j

        div16 match
        {
          case 0 =>
            f = (b & c) | (~b & d) 
          case 1 =>
            f = (b & d) | (c & ~d)
            bufferIndex = (bufferIndex * 5 + 1) & 0x0F 
          case 2 =>
            f = b ^ c ^ d
            bufferIndex = (bufferIndex * 3 + 5) & 0x0F 
          case 3 =>
            f = c ^ (b | ~d)
            bufferIndex = (bufferIndex * 7) & 0x0F
        }

        val temp: Int = b + Integer.rotateLeft(a + f + chunk.get(bufferIndex) + TABLE_T(j), SHIFT_AMTS((div16 << 2) | (j & 3)))
        a = d
        d = c
        c = b
        b = temp
      }
 
      a += originalA
      b += originalB
      c += originalC
      d += originalD
      padded.position(padded.position() + 64)
    }
 
    val md5: ByteBuffer = ByteBuffer.allocate(16).order(ByteOrder.LITTLE_ENDIAN)
    for (n <- Seq(a, b, c, d))
    {
      md5.putInt(n)
    }
    return md5.array()
  }
 
  def toHexString(b: Array[Byte]): String = {
    val sb: StringBuilder = new StringBuilder()
    for (i <- 0 until b.length)
    {
      sb.append("%02X".format(b(i) & 0xFF))
    }
    return sb.toString()
  }
 
  var i = 0

  while (true) {
    computeMD5("".getBytes())

    if (i % 100000 == 0) {
      println(i)
    }

    i += 1
  }
}
