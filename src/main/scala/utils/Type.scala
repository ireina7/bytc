package bytc

import scala.language.implicitConversions
import bytc.Type.JVMType



/**
 * Useful trait to convert type A to T like `Rust`'s  `From`
*/
trait From[A, B]:
    def from(a: A): B

trait Into[A, B]:
    extension (a: A) def into[B]: B


trait FromType[B]:
    inline def from[A]: B




object Type {
    type U1 = Byte
    type U2 = Short
    type U4 = Int

    implicit def IntToU1(i: Int): U1 = i.asInstanceOf[U1]
    implicit def IntToU2(i: Int): U2 = i.asInstanceOf[U2]
    implicit def IntToU4(i: Int): U4 = i.asInstanceOf[U4]
    implicit def CharToU1(c: Char): U1 = c.asInstanceOf[U1]


    case class Var(id: Int, name: String = "")
    object Var:
        def apply(id: Int) = new Var(id)


    case class Version(major: U2, minor: U2)
    case class Path(src: String)

    case class FunctionSignature(accessFlags: U2, descriptorIndex: U2, nameIndex: U2)
    case class FunctionType(accessFlags: U2, retType: String, paramType: String*)


    given Conversion[String, Path] with 
      inline def apply(s: String) = Path(Tool.transformPath(s))

    /**
     * All JVM supported types
    */
    enum JVMType(description: String):
        case I extends JVMType("Int")
        case S extends JVMType("Short")
        case J extends JVMType("Long")
        case C extends JVMType("Char")
        case F extends JVMType("Float")
        case D extends JVMType("Double")
        case Z extends JVMType("Bool") // fake Int
        case V extends JVMType("Void")
        case Arr(elemType: JVMType) extends JVMType(s"[$elemType]")
        case Obj extends JVMType("Object")
        case Function(ps: Seq[JVMType], retType: JVMType)
            extends JVMType(s"${ps.mkString("(", ", ", ")")}=>${retType}")
        
    end JVMType

    
    given FromType[JVMType] with
        override inline def from[T]: JVMType = {
            import scala.compiletime.erasedValue
            import JVMType.{from as _, *}

            inline erasedValue[T] match
                case _: Int       => I
                case _: Short     => S
                case _: Long      => J
                case _: Char      => C
                case _: Float     => F
                case _: Double    => D
                case _: Boolean   => Z
                case _: Unit      => V
                case _: Array[e]  => Arr(from[e])
                case _: (ps => t) => Obj
                case _: Any       => Obj
        }
        

    object JVMType extends FromType[JVMType]:
        override inline def from[T]: JVMType = summon[FromType[JVMType]].from[T]
        def from(s: String) = s match
            case "Int"    => I
            case "Short"  => S
            case "Long"   => J
            case "Char"   => C
            case "Float"  => F
            case "Double" => D
            case "Bool"   => Z
            case "Void"   => V
            case _        => Obj
    end JVMType



    /** Converts a string representing multiple JVM types into a sequence of
     * integers representing their respective sizes in bytes. 
     * */
    def typesToByteCounts(types : String) : Seq[Int] = {
        var s : String = types
        var lst : List[Int] = Nil
        while(!s.isEmpty) {
            val (c, _, ns) = parseRec(s)
            s = ns
            lst = c :: lst
        }
        lst.reverse
    }

    /** Used to compute for instance the stack effect of method invocations or
        * the number of slots required by for method arguments. In reality, a hackish
        * parser. 
        * */
    def typesToByteCount(types : String) : Int = {
        var s : String = types
        var c : Int = 0
        while(!s.isEmpty) {
            val (inc,_,ns) = parseRec(s)
            s = ns
            c += inc
        }
        c
    }

    /** Returns the number of required to store a value of a given type, in JVM
        * notation. */
    def typeToByteCount(tpe : String) : Int = {
        val (c,_,r) = parseRec(tpe)
        if(!r.isEmpty) {
            sys.error("Malformed type (sub)string: " + r)
        }
        c
    }

    private val MethSigRE = """\((.*)\)(.*)""".r
    private def methodSignatureToStackEffect(sig : String) : Int = sig match {
        case MethSigRE(args,ret) => typeToByteCount(ret) - typesToByteCount(args)
        case _ => sys.error("Malformed method signature: " + sig)
    }
    def methodSignatureArgStackEffect(sig: String) : Int = sig match {
        case MethSigRE(args,ret) =>
            typesToByteCount(args) // does not account for 'this' since we don't know if this is static
        case _ => sys.error("Malformed method signature: " + sig)
    }

    // the meat of the parser
    private def parseRec(s : String) : (Int,String,String) = if(s.isEmpty)  (0,s,s) else {
        s.head match {
            case 'B' | 'C' | 'F' | 'I' | 'S' | 'Z' => (1, s.head.toString, s.tail)
            case 'D' | 'J' => (2, s.head.toString, s.tail)
            case 'V' => (0, s.head.toString, s.tail)  // can't really be an argument type.. Oh well.
            case 'L' => {
                val end = s.indexOf(';')
                if(end < 0) sys.error("Malformed type (sub)string: " + s)
                (1, s.substring(0, end), s.substring(end + 1, s.size))
            }
            case '[' => {
                if(s.tail.isEmpty) sys.error("Malformed type string: incomplete array type.")
                val (_, ss, rest) = parseRec(s.tail)
                (1, "[" + ss, rest)
            }
            case _ => sys.error("Malformed type (sub)string: " + s)
        }
    }

    // This one I want to keep internal for now.. I don't think *returning*
    // Strings representing JVM types is good practice. (reading them is OK I'd
    // say).
    def typesToTypesAndBytes(types : String) : Seq[(String, Int)] = {
        var s = types
        var lst: List[(String, Int)] = Nil
        while !s.isEmpty do
            val (c, ss, ns) = parseRec(s)
            s = ns
            lst = (ss, c) :: lst

        lst.reverse
    }
}