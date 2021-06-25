package bytc

import Type._
import cats.data.State

case class InterfaceInfo(interfaceName: String, nameIndex: U2) extends Streamable {
    override def stream = State.modify { stream =>
        stream << nameIndex
    }
}
