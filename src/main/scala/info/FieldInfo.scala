package bytc

import Type._
import cats.data.State

case class FieldInfo(
    var accessFlags: U2, 
    nameIndex: U2, 
    descriptorIndex: U2, 
    attributes: Seq[AttributeInfo]
) extends ByteStreamable {
    
    override def stream = State.modify { stream =>
        stream << accessFlags << nameIndex << descriptorIndex
        stream << attributes.size.asInstanceOf[U2] << attributes
    }
}
