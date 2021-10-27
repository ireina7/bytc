package bytc

import cats.data.State
import cats.*
import cats.syntax.traverse.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*


trait ToStream[F[_], A]:
  extension (a: A) def toStream: F[Unit]

trait Streamable[F[_]]:
  def stream: F[Unit]


given [F[_], T <: Streamable[F]]: ToStream[F, T] with
  extension (x: T) override def toStream = x.stream


type ToByteStream[A]    = ToStream  [[T] =>> State[ByteStream, T], A]
type ByteStreamable     = Streamable[[T] =>> State[ByteStream, T]   ]
type ByteStreamState[A] = State[ByteStream, A]


object ToStream:
  import Type.*

  given ToByteStream[U1] with {
    extension (u1: U1) def toStream = State.modify(_ << u1)
  }
  given ToByteStream[U2] with {
    extension (u2: U2) def toStream = State.modify(_ << u2)
  }
  given ToByteStream[U4] with {
    extension (u4: U4) def toStream = State.modify(_ << u4)
  }
  given ToByteStream[ByteStream] with {
    extension (byteStream: ByteStream) def toStream = State.modify(_ << byteStream)
  }
  given [F[_]: Monad, A](using 
    ev: ToStream[F, A], 
    monoid: Monoid[F[Unit]]
  ): ToStream[F, List[A]] with {

    extension (xs: List[A]) def toStream = 
      xs.map(_.toStream).foldLeft(Monoid[F[Unit]].empty)(_ >> _)
  }

end ToStream


given Monoid[ByteStreamState[Unit]] with {
  def empty = State(s => (s, ()))
  def combine(
    s1: ByteStreamState[Unit], 
    s2: ByteStreamState[Unit]
  ) = s1 >> s2 
}

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
  import Type.*

  private var bytes = new ByteArrayOutputStream
  private var stream: DataOutputStream = new DataOutputStream(bytes)
  def getBytes : Array[Byte] = { stream.flush(); bytes.toByteArray }

  // appends bytes to the stream
  def <<(u1: U1): ByteStream = { stream.write(u1); this }
  def <<(u2: U2): ByteStream = { stream.writeShort(u2); this }
  def <<(u4: U4): ByteStream = { stream.writeInt(u4); this }
  // appends an entire streamable object to the stream
  def <<[A: ToByteStream](streamable: A): ByteStream = streamable.toStream.runS(this).value
  // appends a sequence of streamable objects
  def <<[A: ToByteStream](seq: Seq[A]): ByteStream = seq.traverse(_.toStream).runS(this).value
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