package bytc

import cats.data.State
import cats.Traverse
import cats.syntax.traverse.*



trait ToStream[A]:
    extension (a: A) def toStream: State[ByteStream, Unit]

trait Streamable:
    def stream: State[ByteStream, Unit]


given [T <: Streamable]: ToStream[T] with
    extension (x: T) override def toStream = x.stream



/**
 * ByteStream is the base class to perform real IO side-effects
 * 
 * Never try to create large loops on method `<<`!
*/
class ByteStream {
    import java.io.{
        DataOutputStream,
        ByteArrayOutputStream
    }
    import Type._

    private var bytes = new ByteArrayOutputStream
    private var stream: DataOutputStream = new DataOutputStream(bytes)
    def getBytes : Array[Byte] = { stream.flush(); bytes.toByteArray }

    // appends bytes to the stream
    def <<(u1: U1): ByteStream = { stream.write(u1); this }
    def <<(u2: U2): ByteStream = { stream.writeShort(u2); this }
    def <<(u4: U4): ByteStream = { stream.writeInt(u4); this }
    // appends an entire streamable object to the stream
    def <<[A: ToStream](streamable: A): ByteStream = streamable.toStream.runS(this).value
    // appends a sequence of streamable objects
    def <<[A: ToStream](seq: Seq[A]): ByteStream = seq.traverse(_.toStream).runS(this).value
    // appends an entire other byte stream to the stream
    def <<(bs: ByteStream): ByteStream = {
        bs.stream.flush
        bs.bytes.writeTo(this.stream)
        this
    }

    def size: Int = stream.size

    def writeToFile(fileName: String): Unit = {
        import java.io.FileOutputStream

        val fileStream = new FileOutputStream(fileName)
        stream.flush
        bytes.writeTo(fileStream)
        fileStream.close()
    }
}