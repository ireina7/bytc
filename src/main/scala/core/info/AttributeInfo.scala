package bytc


import Type.*
import cats.data.State

object AttributeInfo {
    def apply(attributeNameIndex: U2, info: Seq[U1]) : AttributeInfo =
        new AttributeInfo(attributeNameIndex, info)

    def unapply(ai: AttributeInfo | Null) : Option[(U2,Seq[U1])] =
        if(ai == null) None else Some((ai.attributeNameIndex, ai.info))
}

class AttributeInfo(val attributeNameIndex: U2, val info: Seq[U1]) extends ByteStreamable {
    override def stream = State.modify { stream =>
        stream << attributeNameIndex
        stream << info.size.asInstanceOf[U4]
        info.foreach(stream << _)
        stream
    }

    def size: Int = 6 + info.size
}


object CodeAttributeInfo {
  def apply(codeNameIndex: U2) : CodeAttributeInfo =
    new CodeAttributeInfo(codeNameIndex)

  def unapply(cai: CodeAttributeInfo | Null) : Option[U2] =
    if(cai == null) None else Some(cai.codeNameIndex)
}

class CodeAttributeInfo(val codeNameIndex: U2) extends AttributeInfo(codeNameIndex, Nil) {
    var maxStack: U2 = 0  // gets set when the code handler 'freezes'
    var maxLocals: U2 = 0 // gets set when the code handler 'freezes'
    var bytes: ByteStream = new ByteStream

    case class ExceptionTableEntry(startPC: U2, endPC: U2, handlerPC: U2, catchType: U2) extends ByteStreamable {
        override def stream = State.pure(())
    }
    var exceptionTable: Seq[ExceptionTableEntry] = Nil
    var attributes: Seq[AttributeInfo] = Nil

    override def stream = State.modify { stream =>
        val codeLength: U4 = bytes.size.asInstanceOf[U4]
        val exceptionTableLength: U2 = exceptionTable.size.asInstanceOf[U2]
        val attributesCount: U2 = attributes.size.asInstanceOf[U2]

        val totalLength = size
        stream << codeNameIndex << (totalLength-6).asInstanceOf[U4] << maxStack << maxLocals << codeLength << bytes
        stream << exceptionTableLength << exceptionTable
        stream << attributesCount << attributes
    }

    private def attributesSize: Int = {
        attributes.foldLeft[Int](0)((s:Int, c:AttributeInfo) => { s + c.size })
    }

    override def size: Int = {
        18 + bytes.size + (exceptionTable.size * 8) + attributesSize
    }
}


class LineNumberTableAttributeInfo(val nameIndex : U2) extends AttributeInfo(nameIndex, Nil) {
    
    private var _entries : Map[Int,Int] = Map.empty

    private def numEntries = _entries.size

    def setEntries(entries : Seq[(Int,Int)]) : Unit = {
        _entries = entries.toMap
    }

    override def stream = State.modify { stream =>
        val ne = numEntries
        // println("Num entries : " + ne)
        // println("Computed    : " + (2 + ne * 4))

        stream << nameIndex << ((2 + ne * 4) : U4) << (ne : U2)
        for((pc,ln) <- _entries.toSeq.sortBy(_._1)) {
        stream << (pc : U2) << (ln : U2)
        }
        stream
    }

    override def size : Int = 8 + numEntries * 4
}