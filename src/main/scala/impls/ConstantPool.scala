package bytc


import Type.*
import cats.data.State

class ConstantPool extends ByteStreamable {
    import scala.collection.mutable.HashMap
    type Index = U2

    def functionTypeToSignature(name: String, functionType: FunctionType): FunctionSignature = {
        val accessFlags: U2 = functionType.accessFlags
        val paramTypes: String = functionType.paramType.mkString("")
        val retType: String = functionType.retType

        val nameIndex: U2 = this.addString(name)
        val descriptorIndex: U2 = this.addString(
            "(" + paramTypes + ")" + retType
        )
        new FunctionSignature(accessFlags, descriptorIndex, nameIndex)
    }

    /** The following methods add constants to the pool, using hashmaps to avoid duplicates 
     * and properly encoding the values. 
     * */
    def addInt(i: Int): Index = intMap.getOrElse(i, {
        val idx = addEntry(CPIntegerInfo(encodeInt(i)))
        intMap += (i -> idx)
        idx
    })
    def addFloat(f: Float): Index = floatMap.getOrElse(f, {
        val idx = addEntry(CPFloatInfo(encodeFloat(f)))
        floatMap += (f -> idx)
        idx
    })

    def addLong(l: Long): Index = longMap.getOrElse(l, {
        val enc = encodeLong(l)
        val idx = addEntry(CPLongInfo(enc._1, enc._2))
        longMap += (l -> idx)
        idx
    })
    def addDouble(d: Double): Index = doubleMap.getOrElse(d, {
        val enc = encodeDouble(d)
        val idx = addEntry(CPDoubleInfo(enc._1, enc._2))
        doubleMap += (d -> idx)
        idx
    })
    def addString(s: String): Index = stringMap.getOrElse(s, {
        val idx = addEntry(CPUtf8Info(encodeString(s)).setSource(s))
        stringMap += (s -> idx)
        idx
    })
    def addStringConstant(strID: U2): U2 = stringConstMap.getOrElse(strID, {
        val idx = addEntry(CPStringInfo(strID))
        stringConstMap += (strID -> idx)
        idx
    })
    def addClass(nameID: U2): U2 = classMap.getOrElse(nameID, {
        val idx = addEntry(CPClassInfo(nameID))
        classMap += (nameID -> idx)
        idx
    })
    def addFieldRef(classID: U2, natID: U2): U2 = fieldRefMap.getOrElse((classID, natID), {
        val idx = addEntry(CPFieldRefInfo(classID,natID))
        fieldRefMap += ((classID,natID) -> idx)
        idx
    })
    def addMethodRef(classID: U2, natID: U2): U2 = methodRefMap.getOrElse((classID, natID), {
        val idx = addEntry(CPMethodRefInfo(classID,natID))
        methodRefMap += ((classID, natID) -> idx)
        idx
    })
    def addInterfaceMethodRef(classID: U2, natID: U2): U2 = methodRefMap.getOrElse((classID, natID), {
        val idx = addEntry(CPInterfaceMethodRefInfo(classID,natID))
        methodRefMap += ((classID, natID) -> idx)
        idx
    })
    def addNameAndType(nameID: U2, typeID: U2): U2 = nameAndTypeMap.getOrElse((nameID, typeID), {
        val idx = addEntry(CPNameAndTypeInfo(nameID,typeID))
        nameAndTypeMap += ((nameID, typeID) -> idx)
        idx
    })

    def getFieldSize(idx: U2): Int = entryAt(idx) match {
        case CPFieldRefInfo(_, natid) => {
            val strDesc: String = entryAt(
                entryAt(natid).asInstanceOf[CPNameAndTypeInfo].descriptorIndex
            ).asInstanceOf[CPUtf8Info].getSource
            strDesc match {
                case "D" | "J" => 2
                case _ => 1
            }
        }
        case _ => sys.error("getFieldSize: no field info at given index.")
    }

    def getMethodEffect(idx: U2): Int = entryAt(idx) match {
        case CPMethodRefInfo(_, natid) => {
            val strDesc: String = entryAt(
                entryAt(natid).asInstanceOf[CPNameAndTypeInfo].descriptorIndex
            ).asInstanceOf[CPUtf8Info].getSource
            //methodSignatureToStackEffect(strDesc)
            ???
        }
        case CPInterfaceMethodRefInfo(_, natid) => {
            val strDesc: String = entryAt(
                entryAt(natid).asInstanceOf[CPNameAndTypeInfo].descriptorIndex
            ).asInstanceOf[CPUtf8Info].getSource
            //methodSignatureToStackEffect(strDesc)
            ???
        }
        case _ => sys.error("getMethodEffect: no method ref info at given index.")
    }

    override def stream: State[ByteStream, Unit] = State.modify { stream =>
        stream << nextIndex.asInstanceOf[Index] << entries
    }


    /** The following maps keep track of the constants already added to the pool to avoid duplicates. */
    private val intMap        : HashMap[Int, U2]      = new HashMap[Int, U2]
    private val floatMap      : HashMap[Float, U2]    = new HashMap[Float, U2]
    private val longMap       : HashMap[Long, U2]     = new HashMap[Long, U2]
    private val doubleMap     : HashMap[Double, U2]   = new HashMap[Double, U2]
    private val stringMap     : HashMap[String, U2]   = new HashMap[String, U2]      // all internal strings
    private val stringConstMap: HashMap[U2, U2]       = new HashMap[U2, U2]          // string constants
    private val classMap      : HashMap[U2, U2]       = new HashMap[U2, U2]
    private val fieldRefMap   : HashMap[(U2, U2), U2] = new HashMap[(U2, U2), U2]
    private val methodRefMap  : HashMap[(U2, U2), U2] = new HashMap[(U2, U2), U2]
    private val nameAndTypeMap: HashMap[(U2, U2), U2] = new HashMap[(U2, U2), U2]

    private var entries: List[CPEntry] = Nil
    private var nextIndex: Index = 1

    /** Adds an entry into the constant pool and returns its index. */
    private def addEntry(entry: CPEntry): Index = {
        entries = entries ::: (entry :: Nil)
        val ret = nextIndex
        nextIndex = nextIndex + (entry match {
            /* Long and Double types need 2 slots
            */
            case e: CPLongInfo   => 2
            case e: CPDoubleInfo => 2
            case _ => 1
        })
        ret
    }
    /** Finds the nth entry. */
    private def entryAt(idx: Int): CPEntry = {
        def search(idx: Int, lst: List[CPEntry]): CPEntry = {
            if(idx == 0) lst.head else lst.head match {
                case e: CPLongInfo   => search(idx - 2, lst.tail)
                case e: CPDoubleInfo => search(idx - 2, lst.tail)
                case _ => search(idx - 1, lst.tail)
            }
        }
        search(idx - 1, entries)
    }

    /** The following methods encode numerical values into their byte representation. */
    private def encodeInt(i: Int): U4 = i
    private def encodeFloat(f: Float): U4 = java.lang.Float.floatToIntBits(f)
    private def encodeLong(l: Long): (U4, U4) = ((l >>> 32).asInstanceOf[U4], (l & 0xFFFFFFFF).asInstanceOf[U4])
    private def encodeDouble(d: Double): (U4, U4) = encodeLong(java.lang.Double.doubleToLongBits(d))

    /** Encodes a string into the unusual UTF8-like encoding used in the class file format. */
    private def encodeString(s: String): Seq[U1] = {
        import scala.collection.mutable.ArrayBuffer
        val bytes = ArrayBuffer.empty[U1]

        for(c: Char <- s) {
            if(c >= 0x0001 && c <= 0x007F) {
                bytes.append(c)
            } else if(c >= 0x0800) {
                bytes.append(0xE0 | ((c >>> 12) & 0x0F))
                bytes.append(0x80 | ((c >>> 6)  & 0x3F))
                bytes.append(0x80 | (         c & 0x3F))
            } else {
                bytes.append(0xC0 | ((c >>> 6) & 0x1F))
                bytes.append(0x80 | (        c & 0x3F))
            }
        }
        bytes.toSeq
    }

}


object CPTags {

    val Class: U1              =  7
    val Fieldref: U1           =  9
    val Methodref: U1          = 10
    val InterfaceMethodref: U1 = 11
    val String: U1             =  8
    val Integer: U1            =  3
    val Float: U1              =  4
    val Long: U1               =  5
    val Double: U1             =  6
    val NameAndType: U1        = 12
    val Utf8: U1               =  1
}

sealed abstract class CPEntry(val tag: U1) extends ByteStreamable

case class CPClassInfo(val nameIndex: U2) extends CPEntry(CPTags.Class) {
    override def stream: State[ByteStream, Unit] = State.modify { stream =>
        stream << tag << nameIndex
    }
}

case class CPFieldRefInfo(val classIndex: U2, nameAndTypeIndex: U2) extends CPEntry(CPTags.Fieldref) {
    override def stream: State[ByteStream, Unit] = State.modify { stream =>
        stream << tag << classIndex << nameAndTypeIndex
    }
}

case class CPMethodRefInfo(val classIndex: U2, nameAndTypeIndex: U2) extends CPEntry(CPTags.Methodref) {
    override def stream: State[ByteStream, Unit] = State.modify { stream =>
        stream << tag << classIndex << nameAndTypeIndex
    }
}

case class CPInterfaceMethodRefInfo(val classIndex: U2, nameAndTypeIndex: U2) extends CPEntry(CPTags.InterfaceMethodref) {
    override def stream: State[ByteStream, Unit] = State.modify { stream =>
        stream << tag << classIndex << nameAndTypeIndex
    }
}

case class CPStringInfo(val stringIndex: U2) extends CPEntry(CPTags.String) {
    override def stream: State[ByteStream, Unit] = State.modify { stream =>
        stream << tag << stringIndex
    }
}

case class CPIntegerInfo(val bytes: U4) extends CPEntry(CPTags.Integer) {
    override def stream: State[ByteStream, Unit] = State.modify { stream =>
        stream << tag << bytes
    }
}

case class CPFloatInfo(val bytes: U4) extends CPEntry(CPTags.Float) {
    override def stream: State[ByteStream, Unit] = State.modify { stream =>
        stream << tag << bytes
    }
}

case class CPLongInfo(val highBytes: U4, val lowBytes: U4) extends CPEntry(CPTags.Long) {
    override def stream: State[ByteStream, Unit] = State.modify { stream =>
        stream << tag << highBytes << lowBytes
    }
}

case class CPDoubleInfo(val highBytes: U4, val lowBytes: U4) extends CPEntry(CPTags.Double) {
    override def stream: State[ByteStream, Unit] = State.modify { stream =>
        stream << tag << highBytes << lowBytes
    }
}

case class CPNameAndTypeInfo(val nameIndex: U2, val descriptorIndex: U2) extends CPEntry(CPTags.NameAndType) {
    override def stream: State[ByteStream, Unit] = State.modify { stream =>
        stream << tag << nameIndex << descriptorIndex
    }
}

case class CPUtf8Info(val bytes: Seq[U1]) extends CPEntry(CPTags.Utf8) {
    private var original: String = _

    def setSource(str: String): CPUtf8Info = { original = str; this }
    def getSource: String = original

    override def stream: State[ByteStream, Unit] = State.modify { stream =>
        stream << tag << bytes.length.asInstanceOf[U2]
        bytes.foreach(b => { stream << b })
        stream
    }
}