package bytc

import Type.*
import cats.data.State

case class InterfaceInfo(interfaceName: String, nameIndex: U2) extends ByteStreamable {
    override def stream = State.modify { stream =>
        stream << nameIndex
    }
}
