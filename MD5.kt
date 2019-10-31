import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.IntBuffer

internal object MD5K {

    private val INIT_A = 0x67452301
    private val INIT_B = 0xEFCDAB89L.toInt()
    private val INIT_C = 0x98BADCFEL.toInt()
    private val INIT_D = 0x10325476

    private val SHIFT_AMTS = intArrayOf(7, 12, 17, 22, 5, 9, 14, 20, 4, 11, 16, 23, 6, 10, 15, 21)

    private val TABLE_T = IntArray(64)

    init {
        for (i in 0..63)
            TABLE_T[i] = ((1L shl 32) * Math.abs(Math.sin((i + 1).toDouble()))).toLong().toInt()
    }

    fun computeMD5(message: ByteArray): ByteArray {
        val padded = ByteBuffer.allocate(((message.size + 8) / 64 + 1) * 64).order(ByteOrder.LITTLE_ENDIAN)
        padded.put(message)
        padded.put(0x80.toByte())
        val messageLenBits = message.size.toLong() * 8
        padded.putLong(padded.capacity() - 8, messageLenBits)

        padded.rewind()

        var a = INIT_A
        var b = INIT_B
        var c = INIT_C
        var d = INIT_D
        while (padded.hasRemaining()) {
            // obtain a slice of the buffer from the current position,
            // and view it as an array of 32-bit ints
            val chunk = padded.slice().order(ByteOrder.LITTLE_ENDIAN).asIntBuffer()
            val originalA = a
            val originalB = b
            val originalC = c
            val originalD = d
            for (j in 0..63) {
                val div16 = j.ushr(4)
                var f = 0
                var bufferIndex = j
                when (div16) {
                    0 -> f = b and c or (b.inv() and d)

                    1 -> {
                        f = b and d or (c and d.inv())
                        bufferIndex = bufferIndex * 5 + 1 and 0x0F
                    }

                    2 -> {
                        f = b xor c xor d
                        bufferIndex = bufferIndex * 3 + 5 and 0x0F
                    }

                    3 -> {
                        f = c xor (b or d.inv())
                        bufferIndex = bufferIndex * 7 and 0x0F
                    }
                }
                val temp = b + Integer.rotateLeft(a + f + chunk.get(bufferIndex) + TABLE_T[j], SHIFT_AMTS[div16 shl 2 or (j and 3)])
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

        val md5 = ByteBuffer.allocate(16).order(ByteOrder.LITTLE_ENDIAN)
        for (n in intArrayOf(a, b, c, d)) {
            md5.putInt(n)
        }
        return md5.array()
    }

    fun toHexString(b: ByteArray): String {
        val sb = StringBuilder()
        for (i in b.indices) {
            sb.append(String.format("%02X", b[i].toInt() and 0xFF))
        }
        return sb.toString()
    }

    @JvmStatic
    fun main(args: Array<String>) {
        var i = 0

        while (true) {
            computeMD5("".toByteArray())

            if (i % 100000 == 0) {
                println(i)
            }

            i++
        }
    }

}
