package bytc

import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Map as MutableMap }
import cats.Monoid
import bytc.Type.FunctionType


class CodeHandler (
    c: CodeAttributeInfo, 
    cp: ConstantPool, 
    functionType: FunctionType,
):
    import Type.*
    import Code.*

    private val code: CodeAttributeInfo = c
    val constantPool: ConstantPool = cp
    private val abcBuffer : ListBuffer[AtomCode] = ListBuffer.empty
    private var frozen : Boolean = false
    private def append(abc: AtomCode): Unit = if !frozen then abcBuffer += abc

    val retType = functionType.retType
    val paramTypes = functionType.paramType.mkString("")
    val isStatic = (functionType.accessFlags & Flag.METHOD_ACC_STATIC) != 0
    
    def <<(ac: AtomCode): CodeHandler = { append(ac); this }

    // Helpers to get slots.
    private val argTypesAndBytes: Seq[(String, Int)] = {
        val bc = typesToTypesAndBytes(paramTypes)
        // that's a dirty trick, but this info is very private..
        // only used for the ArgLoad ABC.
        if isStatic then bc else ("L;", 1) +: bc
    }
    val argSlotMap : Map[Int, (String, Int)] = {
        var acc : Int = 0
        (for(((tpe, sz), arg) <- argTypesAndBytes.zipWithIndex) yield {
            val s = acc
            acc += sz
            (arg, (tpe, s))
        }).toMap
    }
    private var locals: Int = argTypesAndBytes.unzip._2.sum

    def getFreshVar: Int = getFreshVar(1)

    /** Get a slot for a var whose type has the JVM representation `tpe`. */
    def getFreshVar(tpe : String) : Int = getFreshVar(typeToByteCount(tpe))

    /** Get a slot for var that fits in `n` bytes. `n` must be `0` or `1`. */
    def getFreshVar(n : Int) : Int = {
        if !(n == 1 || n == 2) then
            throw new IllegalArgumentException("Slot for variables can only be of 1 or 2 bytes.")
        
        val ret = locals
        locals += n
        ret
    }

    private var labelCounts = new scala.collection.mutable.HashMap[String, Int]
    def getFreshLabel(prefix: String): String = {
        val postfix: Int = labelCounts.getOrElse(prefix, {
            labelCounts(prefix) = 0
            0
        })
        val name = prefix + '_' + postfix
        labelCounts(prefix) = postfix + 1
        name
    }


    def computeMaxStack(abcList : List[AtomCode]) : U2 = {
        assert(frozen)
        50
    }

    /** "Freezes" the code: maxLocals is computed, abstract byte codes are turned
    *  into concrete ones. This includes computation of the label offsets. */
    def freeze(): Unit = if(frozen) {
        throw CodeFreezingException(
        "Cannot invoke `freeze` twice on the same CodeHandler.")
    } else {
        frozen = true

        val abcList = abcBuffer.toList
        code.maxLocals = locals

        val labels: MutableMap[String,Int] = MutableMap.empty
        val lineInfo: MutableMap[Int,Int] = MutableMap.empty

        // In the first pass, we collect the positions of all labels.
        // We also store line numbers information.
        locally {
            var pc: Int = 0
            var lastLineNumber : Int = Int.MaxValue
            for(abc <- abcList) {
                abc match {
                    case LineNumber(ln) if ln == lastLineNumber => ;
                    case LineNumber(ln) => {
                        lastLineNumber = ln
                        lineInfo(pc) = ln
                    }
                    case Label(name) => labels(name) = pc
                    case _ => ;
                }
                pc = pc + abc.size
            }
        }

        // In the second pass, we set the jump offsets.
        locally {
            var pc : Int = 0
            for(abc <- abcList) {
                abc match {
                    case co: Control => {
                        co.offset = (labels.getOrElse(co.goal, 0) - pc)
                    }
                    case _ => ;
                }
                pc = pc + abc.size
            }
        }

        // we build the line number table.
        if(!lineInfo.isEmpty) {
            val lnta = new LineNumberTableAttributeInfo(constantPool.addString("LineNumberTable"))
            lnta.setEntries(lineInfo.toSeq)
            code.attributes = lnta +: code.attributes
        }

        // we now compute the maximum stack height.
        code.maxStack = computeMaxStack(abcList) //Unimplemented!!

        // finally, we dump the code.
        abcList.foreach(code.bytes << _)
    }


    def print(): Unit = if !frozen then
        var pc = 0
        for(abc <- abcBuffer) {
            abc match
                case Label(name) =>
                    println(name + ":")
                case _ =>
                    println("%5d %s".format(pc, abc))
                    pc += abc.size
        }
end CodeHandler



case class CodeFreezingException(
    message : String, code : Code = Monoid[Code].empty
) extends Exception {
    import Code.*

    override def getMessage : String = {
        if(code.isEmpty) {
            message
        } else {
            val b = new StringBuilder()
            b.append(message)
            b.append("\n")
            /*
            var pc = 0
            for(abc <- code.codes) abc match {
                case Label(name) =>
                    b.append(name)
                    b.append(":\n")

                case _ =>
                    b.append("%5d %s".format(pc, abc))
                    b.append("\n")
                    pc += abc.size
            }
            */

            b.toString
        }
    }
}