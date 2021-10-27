package bytc

import Type._
import cats.data.State


case class MethodInfo (
    signature: FunctionSignature,
    attributes: Seq[AttributeInfo]
) extends ByteStreamable {

    var accessFlags: U2 = signature.accessFlags
    val nameIndex: U2 = signature.nameIndex
    val descriptorIndex: U2 = signature.descriptorIndex

    def isStatic : Boolean = (accessFlags & Flag.METHOD_ACC_STATIC) != 0
    override def stream = State.modify { stream =>
        stream << accessFlags << nameIndex << descriptorIndex
        stream << attributes.size.asInstanceOf[U2] << attributes
    }
}


