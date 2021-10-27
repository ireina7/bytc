package bytc


class BytcError(val msg: String) extends Throwable:
  val pos: Int = 0
end BytcError


type Result[A] = Either[BytcError, A]


