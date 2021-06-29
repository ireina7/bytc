package bytc

import scala.language.implicitConversions
import Type.*
import scala.collection.mutable
import mutable.ListBuffer
import cats.Monoid
import cats.Traverse
import cats.Monad
import cats.data.State
//import cats.data.Eval
import cats.syntax.monoid.*
import cats.syntax.traverse.*
import cats.syntax.monad.*



/**
 * Code Geass!
 * Abstract Module of Code
*/
trait Geass[Code](using monoid: Monoid[Code]):
    import ByteCode.*
    import AtomCode.*

    extension (code: Code) def <<(other: Code): Code = code |+| other

    def define(name: String)(codes: Seq[Code]): Code =
        codes.foldLeft[Code](comment(s"Define $name"))((acc, code) => acc << code)

    def atom(atomCode: AtomCode): Code
    def byte(byteCode: ByteCode): Code

    given Conversion[AtomCode, Code] = atom
    given Conversion[ByteCode, Code] = byte

    def pass: Code = monoid.empty
    def comment(s: String) = pass

    // load value
    trait Loadable[A]:
        extension (a: A) def load: Code
    def load[A: Loadable](x: A): Code = x.load

    // store value
    def store(x: Var): Code

    // Instance creation
    def `new`(className: Path): Code
    def defaultNew(className: Path): Code = `new`(className)
        << DUP
        << invokeSpecial(className, "<init>", "()V")

    // return
    def `return`: Code

    // Gets and Puts
    protected def accessField
        (bc: ByteCode, className: String, fieldName: String, fieldType: String): Code

    def getField(className: Path, fieldName: String, fieldType: String) = 
        accessField(GETFIELD,  className.src, fieldName, fieldType)
    def getStatic(className: Path, fieldName: String, fieldType: String) = 
        accessField(GETSTATIC, className.src, fieldName, fieldType)
    def putField(className: Path, fieldName: String, fieldType: String) = 
        accessField(PUTFIELD,  className.src, fieldName, fieldType)
    def putStatic(className: Path, fieldName: String, fieldType: String) = 
        accessField(PUTSTATIC, className.src, fieldName, fieldType)


    // Invoke
    protected def invokeMethod
        (bc: ByteCode, className: String, methodName: String, methodSig: String): Code
    
    def invokeSpecial(className: Path, methodName: String, methodSig: String): Code = 
        invokeMethod(INVOKESPECIAL, className.src, methodName, methodSig)
    def InvokeStatic(className: Path, methodName: String, methodSig: String): Code =
        invokeMethod(INVOKESTATIC, className.src, methodName, methodSig)
    def InvokeVirtual(className: Path, methodName: String, methodSig: String): Code =
        invokeMethod(INVOKEVIRTUAL, className.src, methodName, methodSig)
    def InvokeInterface(className: Path, methodName: String, methodSig: String): Code = 
        invokeMethod(INVOKEINTERFACE, className.src, methodName, methodSig)
            << RawByte(methodSignatureArgStackEffect(methodSig) + 1)
            << RawByte(0)
            
end Geass














class Code(val state: State[CodeHandler, Unit]):
    val size: Int = 0
    def isEmpty: Boolean = size == 0
    def run(ch: CodeHandler) = state.run(ch).value
    def runS(ch: CodeHandler) = state.runS(ch).value
    def apply(ch: CodeHandler) = runS(ch)
    def <<(other: Code) = this |+| other
end Code

given Monoid[Code] with
    def empty = new Code(State.pure(()))
    def combine(x: Code, y: Code) = new Code(x.state.flatMap(_ => y.state))






given Conversion[AtomCode, Code] with
    def apply(atom: AtomCode): Code = new Code(State.modify(_ << atom))

trait AtomCode(val size: Int)

object AtomCode:
    import ByteCode.*

    case object Empty extends AtomCode(0)
    case class Raw(byteCode: ByteCode) extends AtomCode(1) {
        override def toString = byteCode.toString
    }
    case class LineNumber(line : Int) extends AtomCode(0)
    case class Label(id: String) extends AtomCode(0)
    case class RawByte(u1: U1) extends AtomCode(1)
    case class RawBytes(u2: U2) extends AtomCode(2)
    class Control(val op: ByteCode, val goal: String) extends AtomCode(3) {
        var offset: Int = 0
    }
    case class Goto(override val goal: String) extends Control(GOTO, goal)
    case class IfEq(override val goal: String) extends Control(IFEQ, goal)
    case class IfNe(override val goal: String) extends Control(IFNE, goal)
    case class IfLt(override val goal: String) extends Control(IFLT, goal)
    case class IfLe(override val goal: String) extends Control(IFLE, goal)
    case class IfGt(override val goal: String) extends Control(IFGT, goal)
    case class IfGe(override val goal: String) extends Control(IFGE, goal)
    case class IfNull(override val goal: String) extends Control(IFNULL, goal)
    case class IfNonNull(override val goal: String) extends Control(IFNONNULL, goal)
    case class If_ICmpEq(override val goal: String) extends Control(IF_ICMPEQ, goal)
    case class If_ICmpNe(override val goal: String) extends Control(IF_ICMPNE, goal)
    case class If_ICmpLt(override val goal: String) extends Control(IF_ICMPLT, goal)
    case class If_ICmpLe(override val goal: String) extends Control(IF_ICMPLE, goal)
    case class If_ICmpGt(override val goal: String) extends Control(IF_ICMPGT, goal)
    case class If_ICmpGe(override val goal: String) extends Control(IF_ICMPGE, goal)
    case class If_ACmpEq(override val goal: String) extends Control(IF_ACMPEQ, goal)
    case class If_ACmpNe(override val goal: String) extends Control(IF_ACMPNE, goal)

end AtomCode


given ToStream[AtomCode] with
    extension (atomCode: AtomCode) def toStream =
        import AtomCode.*
        atomCode match
            case Raw(byteCode) => State.modify(_ << byteCode.code)
            case RawByte(b) => State.modify(_ << b)
            case RawBytes(bs) => State.modify(_ << bs)
            case ctl: Control => State.modify { bs =>
                if ctl.offset > 65536 || ctl.offset < -32768 then
                    sys.error("Unsupported long jump." + this)
                else
                    bs << ctl.op << ctl.offset.asInstanceOf[U2]
            }
            case _ => State.pure(())
    end extension





object Code:
    export ByteCode.*
    export AtomCode.*

    given Conversion[ByteCode, AtomCode] with
        def apply(b: ByteCode) = AtomCode.Raw(b)

    private inline def modify(f: CodeHandler => CodeHandler): Code = new Code(State.modify(f))
    
    def atom(a: AtomCode): Code = new Code(State.modify(_ << a))

    def define(name: String)(codes: List[Code]): Code =
        codes.foldLeft[Code](Comment(s"Define $name"))((acc, code) => acc << code)

    case object Pass extends Code(State.pure(())):
        override val size = 0
    
    /** Generates code to load constants, using the appropriate method depending on the values. */
    object Load:
        private def ldc_ref(cpRef: U2): Code = modify { ch => 
            if cpRef <= 0xFF then ch << LDC << RawByte((cpRef & 0xFF).asInstanceOf[U1])
            else ch << LDC_W << RawBytes(cpRef)
        }
        private def ldc2_ref(cpRef: U2): Code = modify { ch => 
            ch << LDC2_W << RawBytes(cpRef)  
        }

        def apply(i: Int): Code = i match 
            case -1 => modify(_ << ICONST_M1)
            case 0  => modify(_ << ICONST_0)
            case 1  => modify(_ << ICONST_1)
            case 2  => modify(_ << ICONST_2)
            case 3  => modify(_ << ICONST_3)
            case 4  => modify(_ << ICONST_4)
            case 5  => modify(_ << ICONST_5)
            case _ if(i >= -128   && i <= 127  ) => modify(_ << BIPUSH << RawByte(i.asInstanceOf[U1]))
            case _ if(i >= -32768 && i <= 32767) => modify(_ << SIPUSH << RawBytes(i.asInstanceOf[U2]))
            case _ => modify { ch => ldc_ref(ch.constantPool.addInt(i)).runS(ch) }


        def apply(f: Float): Code = f match 
            case 0.0f => modify(_ << FCONST_0)
            case 1.0f => modify(_ << FCONST_1)
            case 2.0f => modify(_ << FCONST_2)
            case _ => modify(ch => ldc_ref(ch.constantPool.addFloat(f)).runS(ch))


        def apply(d: Double): Code = d match
            case 0.0 => modify(_ << DCONST_0)
            case 1.0 => modify(_ << DCONST_1)
            case _ => modify(ch => ldc2_ref(ch.constantPool.addDouble(d)).runS(ch))
        

        def apply(l: Long): Code = l match
            case 0l => modify(_ << LCONST_0)
            case 1l => modify(_ << LCONST_1)
            case _ => modify(ch => ldc2_ref(ch.constantPool.addLong(l)).runS(ch))
        

        def apply(s: String): Code = modify { ch => 
            ldc_ref(ch.constantPool.addStringConstant(ch.constantPool.addString(s))).runS(ch)
        }

        def apply(c: Class[_]): Code = modify { ch => 
            ldc_ref(ch.constantPool.addClass(ch.constantPool.addString(c.getName().replaceAll("\\.", "/"))))
                .runS(ch)
        }

        def apply(arg: Var): Code = ArgLoad(arg.id)
        
    end Load

    object Store:
        def apply(arg: Var) = ArgStore(arg.id)
    end Store

    // IINC business
    object IInc:
        def apply(index: Int, inc: Int): Code = modify { ch => 
            if(index <= 127 && inc >= -128 && inc <= 127) {
                ch << IINC << RawByte(index.asInstanceOf[U1]) << RawByte(inc.asInstanceOf[U1])
            } else if(index <= 32767 && inc >= -32768 && inc <= 32767) {
                ch << WIDE << IINC << RawBytes(index.asInstanceOf[U2]) << RawBytes(index.asInstanceOf[U2])
            } else {
                sys.error("Index or increment too large in IInc " + index + " " + inc)
            }
        }
    end IInc

    // Loading and storing locals
    private def storeLoad(
        index: Int, name: String, bc: ByteCode,
        bc0: ByteCode, bc1: ByteCode,
        bc2: ByteCode, bc3: ByteCode
    ): Code = {

        modify { ch => index match
            case 0 => ch << bc0
            case 1 => ch << bc1
            case 2 => ch << bc2
            case 3 => ch << bc3
            case _ if(index >= 0 && index <= 127) => ch << bc << RawByte(index.asInstanceOf[U1])
            case _ if(index >= 0 && index <= 32767) => ch << WIDE << bc << RawBytes(index.asInstanceOf[U2])
            case _ => sys.error("Invalid index in " + name + " " + index)
        }
    }
    object ALoad { def apply(index: Int) = storeLoad(index, "ALoad", ALOAD, ALOAD_0, ALOAD_1, ALOAD_2, ALOAD_3) }
    object DLoad { def apply(index: Int) = storeLoad(index, "DLoad", DLOAD, DLOAD_0, DLOAD_1, DLOAD_2, DLOAD_3) }
    object FLoad { def apply(index: Int) = storeLoad(index, "FLoad", FLOAD, FLOAD_0, FLOAD_1, FLOAD_2, FLOAD_3) }
    object ILoad { def apply(index: Int) = storeLoad(index, "ILoad", ILOAD, ILOAD_0, ILOAD_1, ILOAD_2, ILOAD_3) }
    object LLoad { def apply(index: Int) = storeLoad(index, "LLoad", LLOAD, LLOAD_0, LLOAD_1, LLOAD_2, LLOAD_3) }
    object AStore { def apply(index: Int) = storeLoad(index, "AStore", ASTORE, ASTORE_0, ASTORE_1, ASTORE_2, ASTORE_3) }
    object DStore { def apply(index: Int) = storeLoad(index, "DStore", DSTORE, DSTORE_0, DSTORE_1, DSTORE_2, DSTORE_3) }
    object FStore { def apply(index: Int) = storeLoad(index, "FStore", FSTORE, FSTORE_0, FSTORE_1, FSTORE_2, FSTORE_3) }
    object IStore { def apply(index: Int) = storeLoad(index, "IStore", ISTORE, ISTORE_0, ISTORE_1, ISTORE_2, ISTORE_3) }
    object LStore { def apply(index: Int) = storeLoad(index, "LStore", LSTORE, LSTORE_0, LSTORE_1, LSTORE_2, LSTORE_3) }
    
    object ArgLoad:
        /** Loads an argument by its index in the argument list. 0 is the receiver
         * for non-static methods. */
        def apply(index : Int) : Code = modify { ch => 
            ch.argSlotMap.get(index) match {
                case None => sys.error("Invalid argument index : " + index)
                case Some((t, i)) => t match 
                    case "I" | "B" | "C" | "S" | "Z" => ILoad(i).runS(ch)
                    case "F" => FLoad(i).runS(ch)
                    case "J" => LLoad(i).runS(ch)
                    case "D" => DLoad(i).runS(ch)
                    case "V" => sys.error("Illegal argument of type `void` !?!")
                    case _  => ALoad(i).runS(ch) // this is bold :)
            }
        }
    end ArgLoad

    object ArgStore:
        /** Loads an argument by its index in the argument list. 0 is the receiver
         * for non-static methods. */
        def apply(index : Int) : Code = modify { ch => 
            ch.argSlotMap.get(index) match {
                case None => sys.error("Invalid argument index : " + index)
                case Some((t, i)) => t match 
                    case "I" | "B" | "C" | "S" | "Z" => IStore(i).runS(ch)
                    case "F" => FStore(i).runS(ch)
                    case "J" => LStore(i).runS(ch)
                    case "D" => DStore(i).runS(ch)
                    case "V" => sys.error("Illegal argument of type `void` !?!")
                    case _  => AStore(i).runS(ch) // this is bold :)
            }
        }
    end ArgStore

    // Field access
    object GetField  { def apply(className: Path, fieldName: String, fieldType: String) = accessField(GETFIELD,  className.src, fieldName, fieldType) }
    object GetStatic { def apply(className: Path, fieldName: String, fieldType: String) = accessField(GETSTATIC, className.src, fieldName, fieldType) }
    object PutField  { def apply(className: Path, fieldName: String, fieldType: String) = accessField(PUTFIELD,  className.src, fieldName, fieldType) }
    object PutStatic { def apply(className: Path, fieldName: String, fieldType: String) = accessField(PUTSTATIC, className.src, fieldName, fieldType) }

    private def accessField(bc: ByteCode, className: String, fieldName: String, fieldType: String): Code =
        modify { ch => {
            ch << bc << RawBytes(ch.constantPool.addFieldRef(
                ch.constantPool.addClass(ch.constantPool.addString(className)),
                ch.constantPool.addNameAndType(
                ch.constantPool.addString(fieldName),
                ch.constantPool.addString(fieldType))))
            }
        }
    
    // Method invocations

    object InvokeInterface:
        def apply(className: Path, methodName: String, methodSig: String): Code = 
            modify { ch => 
                invokeMethod(INVOKEINTERFACE, className.src, methodName, methodSig).runS(ch) << 
                    RawByte(methodSignatureArgStackEffect(methodSig) + 1) << 
                    RawByte(0)
            }


    object InvokeSpecial:
        def apply(className: Path, methodName: String, methodSig: String): Code =
            invokeMethod(INVOKESPECIAL, className.src, methodName, methodSig)


    object InvokeStatic:
        def apply(className: Path, methodName: String, methodSig: String): Code =
            invokeMethod(INVOKESTATIC, className.src, methodName, methodSig)


    object InvokeVirtual:
        def apply(className: Path, methodName: String, methodSig: String): Code =
            invokeMethod(INVOKEVIRTUAL, className.src, methodName, methodSig)


    private def invokeMethod(bc: ByteCode, className: String, methodName: String, methodSig: String): Code = 
        modify { ch => 
            val addMethodRef = if bc == INVOKEINTERFACE 
                then ch.constantPool.addInterfaceMethodRef
                else ch.constantPool.addMethodRef
            
            ch << bc << RawBytes(addMethodRef(
                ch.constantPool.addClass(ch.constantPool.addString(className)),
                ch.constantPool.addNameAndType(
                    ch.constantPool.addString(methodName),
                    ch.constantPool.addString(methodSig))
                ))
        }


    // misc

    object New:
        def apply(className: Path) : Code = modify { ch => 
            ch << NEW << RawBytes(ch.constantPool.addClass(ch.constantPool.addString(className.src)))
        }

    object DefaultNew:
        def apply(className: Path): Code = modify { ch => 
            ch << NEW << RawBytes(ch.constantPool.addClass(ch.constantPool.addString(className.src))) << DUP 
            InvokeSpecial(className, "<init>", "()V").runS(ch)
        }


    object InstanceOf:
        def apply(className: Path): Code = modify { ch => 
            ch << INSTANCEOF << RawBytes(ch.constantPool.addClass(ch.constantPool.addString(className.src)))
        }

    object CheckCast:
        def apply(className: Path): Code = modify { ch => 
            ch << CHECKCAST << RawBytes(ch.constantPool.addClass(ch.constantPool.addString(className.src)))
        }

    object NewArray:
        def apply(arrayType: String): Code = modify { ch => 
            ch << ANEWARRAY << RawBytes(ch.constantPool.addClass(ch.constantPool.addString(arrayType)))
        }

        def apply(tpe: Int): Code = modify { ch => 
            ch << NEWARRAY << RawByte(tpe)
        }

        def primitive(tpe: String): Code = { // with type string
            apply(types(tpe))
        }

        val types = Map(
            "T_BOOLEAN" -> 4,
            "T_CHAR"    -> 5,
            "T_FLOAT"   -> 6,
            "T_DOUBLE"  -> 7,
            "T_BYTE"    -> 8,
            "T_SHORT"   -> 9,
            "T_INT"     -> 10,
            "T_LONG"    -> 11
        )
    end NewArray


    case object Return extends Code(State.modify { ch =>
        val code = ch.retType match
            case "I" | "B" | "C" | "S" | "Z" => IRETURN
            case "F" => FRETURN
            case "D" => DRETURN
            case "J" => LRETURN
            case "V" => RETURN
            case obj => ARETURN
        ch << code
    }) { override val size = 1 }

    // So that we can annotate our generated code before freezing
    case class Comment(comment: String) extends Code(State.pure(())):
        override val size: Int = 0

    case object PrintCode extends Code(State.modify { ch =>
        ch.print()
        ch
    })

    case class Inspect(inpect: CodeHandler => Unit) extends Code(State.modify {
        ch => inpect(ch); ch
    })

    case class NewCode(inpect: CodeHandler => CodeHandler) extends Code(State.modify {
        ch => inpect(ch)
    })

end Code





given Conversion[ByteCode, Code] with
    def apply(byteCode: ByteCode): Code = new Code(State.modify(_ << AtomCode.Raw(byteCode)))

given Conversion[Int, Option[Int]] with
    def apply(i: Int): Option[Int] = Some(i)

enum ByteCode(val code: U1, se: Option[Int], l: Option[Int]) extends Streamable:
    val stackEffect: Option[Int] = se
    val length: Option[Int] = l
    override def stream = State.modify(_ << code)

    case AALOAD extends ByteCode(0x32, -1, 1)
    case AASTORE extends ByteCode(0x53, -3, 1)
    case ACONST_NULL extends ByteCode(0x1, 1, 1)
    case ALOAD_0 extends ByteCode(0x2A, 1, 1)
    case ALOAD_1 extends ByteCode(0x2B, 1, 1)
    case ALOAD_2 extends ByteCode(0x2C, 1, 1)
    case ALOAD_3 extends ByteCode(0x2D, 1, 1)
    case ALOAD extends ByteCode(0x19, 1, 2)
    case ANEWARRAY extends ByteCode(0xBD, 0, 3)
    case ARETURN extends ByteCode(0xB0, -1, 1)
    case ARRAYLENGTH extends ByteCode(0xBE, 0, 1)
    case ASTORE_0 extends ByteCode(0x4B, -1, 1)
    case ASTORE_1 extends ByteCode(0x4C, -1, 1)
    case ASTORE_2 extends ByteCode(0x4D, -1, 1)
    case ASTORE_3 extends ByteCode(0x4E, -1, 1)
    case ASTORE extends ByteCode(0x3A, -1, 2)
    case ATHROW extends ByteCode(0xBF, -1, 1)
    case BALOAD extends ByteCode(0x33, -1, 1)
    case BASTORE extends ByteCode(0x54, -3, 1)
    case BIPUSH extends ByteCode(0x10, 1, 2)
    case CALOAD extends ByteCode(0x34, -1, 1)
    case CASTORE extends ByteCode(0x55, -3, 1)
    case CHECKCAST extends ByteCode(0xC0, 0, 3)
    case D2F extends ByteCode(0x90, -1, 1)
    case D2I extends ByteCode(0x8E, -1, 1)
    case D2L extends ByteCode(0x8F, 0, 1)
    case DADD extends ByteCode(0x63, -2, 1)
    case DALOAD extends ByteCode(0x31, 0, 1)
    case DASTORE extends ByteCode(0x52, -4, 1)
    case DCMPG extends ByteCode(0x98, -3, 1)
    case DCMPL extends ByteCode(0x97, -3, 1)
    case DCONST_0 extends ByteCode(0xE, 2, 1)
    case DCONST_1 extends ByteCode(0xF, 2, 1)
    case DDIV extends ByteCode(0x6F, -2, 1)
    case DLOAD_0 extends ByteCode(0x26, 2, 1)
    case DLOAD_1 extends ByteCode(0x27, 2, 1)
    case DLOAD_2 extends ByteCode(0x28, 2, 1)
    case DLOAD_3 extends ByteCode(0x29, 2, 1)
    case DLOAD extends ByteCode(0x18, 2, 2)
    case DMUL extends ByteCode(0x6B, -2, 1)
    case DNEG extends ByteCode(0x77, 0, 1)
    case DREM extends ByteCode(0x73, -2, 1)
    case DRETURN extends ByteCode(0xAF, -2, 1)
    case DSTORE_0 extends ByteCode(0x47, -2, 1)
    case DSTORE_1 extends ByteCode(0x48, -2, 1)
    case DSTORE_2 extends ByteCode(0x49, -2, 1)
    case DSTORE_3 extends ByteCode(0x4A, -2, 1)
    case DSTORE extends ByteCode(0x39, -2, 2)
    case DSUB extends ByteCode(0x67, -2, 1)
    case DUP2 extends ByteCode(0x5C, 2, 1)
    case DUP2_X1 extends ByteCode(0x5D, 2, 1)
    case DUP2_X2 extends ByteCode(0x5E, 2, 1)
    case DUP extends ByteCode(0x59, 1, 1)
    case DUP_X1 extends ByteCode(0x5A, 1, 1)
    case DUP_X2 extends ByteCode(0x5B, 1, 1)
    case F2D extends ByteCode(0x8D, 1, 1)
    case F2I extends ByteCode(0x8B, 0, 1)
    case F2L extends ByteCode(0x8C, 1, 1)
    case FADD extends ByteCode(0x62, -1, 1)
    case FALOAD extends ByteCode(0x30, -1, 1)
    case FASTORE extends ByteCode(0x51, -3, 1)
    case FCMPG extends ByteCode(0x96, -1, 1)
    case FCMPL extends ByteCode(0x95, -1, 1)
    case FCONST_0 extends ByteCode(0xB, 1, 1)
    case FCONST_1 extends ByteCode(0xC, 1, 1)
    case FCONST_2 extends ByteCode(0xD, 1, 1)
    case FDIV extends ByteCode(0x6E, -1, 1)
    case FLOAD_0 extends ByteCode(0x22, 1, 1)
    case FLOAD_1 extends ByteCode(0x23, 1, 1)
    case FLOAD_2 extends ByteCode(0x24, 1, 1)
    case FLOAD_3 extends ByteCode(0x25, 1, 1)
    case FLOAD extends ByteCode(0x17, 1, 2)
    case FMUL extends ByteCode(0x6A, -1, 1)
    case FNEG extends ByteCode(0x76, 0, 1)
    case FREM extends ByteCode(0x72, -1, 1)
    case FRETURN extends ByteCode(0xAE, -1, 1)
    case FSTORE_0 extends ByteCode(0x43, -1, 1)
    case FSTORE_1 extends ByteCode(0x44, -1, 1)
    case FSTORE_2 extends ByteCode(0x45, -1, 1)
    case FSTORE_3 extends ByteCode(0x46, -1, 1)
    case FSTORE extends ByteCode(0x38, -1, 2)
    case FSUB extends ByteCode(0x66, -1, 1)
    case GETFIELD extends ByteCode(0xB4, None, 3)
    case GETSTATIC extends ByteCode(0xB2, None, 3)
    case GOTO extends ByteCode(0xA7, 0, 3)
    case GOTO_W extends ByteCode(0xC8, 0, 5)
    case I2B extends ByteCode(0x91, 0, 1)
    case I2C extends ByteCode(0x92, 0, 1)
    case I2D extends ByteCode(0x87, 1, 1)
    case I2F extends ByteCode(0x86, 0, 1)
    case I2L extends ByteCode(0x85, 1, 1)
    case I2S extends ByteCode(0x93, 0, 1)
    case IADD extends ByteCode(0x60, -1, 1)
    case IALOAD extends ByteCode(0x2E, -1, 1)
    case IAND extends ByteCode(0x7E, -1, 1)
    case IASTORE extends ByteCode(0x4F, -3, 1)
    case ICONST_0 extends ByteCode(0x3, 1, 1)
    case ICONST_1 extends ByteCode(0x4, 1, 1)
    case ICONST_2 extends ByteCode(0x5, 1, 1)
    case ICONST_3 extends ByteCode(0x6, 1, 1)
    case ICONST_4 extends ByteCode(0x7, 1, 1)
    case ICONST_5 extends ByteCode(0x8, 1, 1)
    case ICONST_M1 extends ByteCode(0x2, 1, 1)
    case IDIV extends ByteCode(0x6C, -1, 1)
    case IF_ACMPEQ extends ByteCode(0xA5, -2, 3)
    case IF_ACMPNE extends ByteCode(0xA6, -2, 3)
    case IFEQ extends ByteCode(0x99, -1, 3)
    case IFGE extends ByteCode(0x9C, -1, 3)
    case IFGT extends ByteCode(0x9D, -1, 3)
    case IF_ICMPEQ extends ByteCode(0x9F, -2, 3)
    case IF_ICMPGE extends ByteCode(0xA2, -2, 3)
    case IF_ICMPGT extends ByteCode(0xA3, -2, 3)
    case IF_ICMPLE extends ByteCode(0xA4, -2, 3)
    case IF_ICMPLT extends ByteCode(0xA1, -2, 3)
    case IF_ICMPNE extends ByteCode(0xA0, -2, 3)
    case IFLE extends ByteCode(0x9E, -1, 3)
    case IFLT extends ByteCode(0x9B, -1, 3)
    case IFNE extends ByteCode(0x9A, -1, 3)
    case IFNONNULL extends ByteCode(0xC7, -1, 3)
    case IFNULL extends ByteCode(0xC6, -1, 3)
    case IINC extends ByteCode(0x84, 0, 3)
    case ILOAD_0 extends ByteCode(0x1A, 1, 1)
    case ILOAD_1 extends ByteCode(0x1B, 1, 1)
    case ILOAD_2 extends ByteCode(0x1C, 1, 1)
    case ILOAD_3 extends ByteCode(0x1D, 1, 1)
    case ILOAD extends ByteCode(0x15, 1, 2)
    case IMUL extends ByteCode(0x68, -1, 1)
    case INEG extends ByteCode(0x74, 0, 1)
    case INSTANCEOF extends ByteCode(0xC1, 0, 3)
    case INVOKEINTERFACE extends ByteCode(0xB9, None, 5)
    case INVOKESPECIAL extends ByteCode(0xB7, None, 3)
    case INVOKESTATIC extends ByteCode(0xB8, None, 3)
    case INVOKEVIRTUAL extends ByteCode(0xB6, None, 3)
    case IOR extends ByteCode(0x80, -1, 1)
    case IREM extends ByteCode(0x70, -1, 1)
    case IRETURN extends ByteCode(0xAC, -1, 1)
    case ISHL extends ByteCode(0x78, -1, 1)
    case ISHR extends ByteCode(0x7A, -1, 1)
    case ISTORE_0 extends ByteCode(0x3B, -1, 1)
    case ISTORE_1 extends ByteCode(0x3C, -1, 1)
    case ISTORE_2 extends ByteCode(0x3D, -1, 1)
    case ISTORE_3 extends ByteCode(0x3E, -1, 1)
    case ISTORE extends ByteCode(0x36, -1, 2)
    case ISUB extends ByteCode(0x64, -1, 1)
    case IUSHR extends ByteCode(0x7C, -1, 1)
    case IXOR extends ByteCode(0x82, -1, 1)
    case JSR extends ByteCode(0xA8, 1, 3)
    case JSR_W extends ByteCode(0xC9, 1, 5)
    case L2D extends ByteCode(0x8A, 0, 1)
    case L2F extends ByteCode(0x89, -1, 1)
    case L2I extends ByteCode(0x88, -1, 1)
    case LADD extends ByteCode(0x61, -2, 1)
    case LALOAD extends ByteCode(0x2F, 0, 1)
    case LAND extends ByteCode(0x7F, -2, 1)
    case LASTORE extends ByteCode(0x50, -4, 1)
    case LCMP extends ByteCode(0x94, -3, 1)
    case LCONST_0 extends ByteCode(0x9, 2, 1)
    case LCONST_1 extends ByteCode(0xA, 2, 1)
    case LDC2_W extends ByteCode(0x14, 2, 3)
    case LDC extends ByteCode(0x12, 1, 2)
    case LDC_W extends ByteCode(0x13, 1, 3)
    case LDIV extends ByteCode(0x6D, -2, 1)
    case LLOAD_0 extends ByteCode(0x1E, 2, 1)
    case LLOAD_1 extends ByteCode(0x1F, 2, 1)
    case LLOAD_2 extends ByteCode(0x20, 2, 1)
    case LLOAD_3 extends ByteCode(0x21, 2, 1)
    case LLOAD extends ByteCode(0x16, 2, 2)
    case LMUL extends ByteCode(0x69, -2, 1)
    case LNEG extends ByteCode(0x75, 0, 1)
    case LOOKUPSWITCH extends ByteCode(0xAB, -1, None)
    case LOR extends ByteCode(0x81, -2, 1)
    case LREM extends ByteCode(0x71, -2, 1)
    case LRETURN extends ByteCode(0xAD, -2, 1)
    case LSHL extends ByteCode(0x79, -1, 1)
    case LSHR extends ByteCode(0x7B, -1, 1)
    case LSTORE_0 extends ByteCode(0x3F, -2, 1)
    case LSTORE_1 extends ByteCode(0x40, -2, 1)
    case LSTORE_2 extends ByteCode(0x41, -2, 1)
    case LSTORE_3 extends ByteCode(0x42, -2, 1)
    case LSTORE extends ByteCode(0x37, -2, 2)
    case LSUB extends ByteCode(0x65, -2, 1)
    case LUSHR extends ByteCode(0x7D, -1, 1)
    case LXOR extends ByteCode(0x83, -2, 1)
    case MONITORENTER extends ByteCode(0xC2, -1, 1)
    case MONITOREXIT extends ByteCode(0xC3, -1, 1)
    case MULTIANEWARRAY extends ByteCode(0xC5, None, 4)
    case NEWARRAY extends ByteCode(0xBC, 0, 2)
    case NEW extends ByteCode(0xBB, 1, 3)
    case NOP extends ByteCode(0x0, 0, 1)
    case POP2 extends ByteCode(0x58, -2, 1)
    case POP extends ByteCode(0x57, -1, 1)
    case PUTFIELD extends ByteCode(0xB5, None, 3)
    case PUTSTATIC extends ByteCode(0xB3, None, 3)
    case RET extends ByteCode(0xA9, 0, 2)
    case RETURN extends ByteCode(0xB1, 0, 1)
    case SALOAD extends ByteCode(0x35, -1, 1)
    case SASTORE extends ByteCode(0x56, -3, 1)
    case SIPUSH extends ByteCode(0x11, 1, 3)
    case SWAP extends ByteCode(0x5F, 0, 1)
    case TABLESWITCH extends ByteCode(0xAA, -1, None)
    case WIDE extends ByteCode(0xC4, None, None)

end ByteCode
