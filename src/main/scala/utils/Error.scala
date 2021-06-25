package bytc


class BytcError(val msg: String):
  val pos: Int = 0
end BytcError


type Result[A] = Either[BytcError, A]


