package bytc

import scala.language.implicitConversions
import cats.data.State


class ClassFile(val className: String, superName: Option[String] = None) extends Streamable {
    import Type._
    import Defaults._

    private val magic: U4 = defaultMagic
    private val minor: U2 = defaultMinor
    private val major: U2 = defaultMajor

    private var constantPool = new ConstantPool()
    lazy val codeNameIndex: U2 = constantPool.addString("Code")
    lazy val sourceFileNameIndex: U2 = constantPool.addString("SourceFile")

    private var accessFlags: U2 = defaultClassAccessFlags
    private val thisClass: U2 = constantPool.addClass(constantPool.addString(className))

    private val superClassName: String = superName match {
        case None => "java/lang/Object"
        case Some(name) => name
    }
    private var superClass: U2 = constantPool.addClass(constantPool.addString(superClassName))

    private var fields     : List[FieldInfo]     = Nil
    private var methods    : List[MethodInfo]    = Nil
    private var interfaces : List[InterfaceInfo] = Nil
    private var attributes : List[AttributeInfo] = Nil
    
    def addInterface(name: String) = {
        val nameIndex = constantPool.addClass(constantPool.addString(name))
        interfaces = InterfaceInfo(name, nameIndex) :: interfaces
    }

    private var _srcNameWasSet = false
    /** Attaches the name of the original source file to the class file. */
    /*
    def setSourceFile(sf : String) : Unit = {
        if(_srcNameWasSet) {
            sys.error("Cannot set the source file attribute twice.")
        }
        _srcNameWasSet = true
        val idx = constantPool.addString(sf)
        attributes = SourceFileAttributeInfo(sourceFileNameIndex, idx) :: attributes
    }
    */

    /** Sets the access flags for the class. */
    def setFlags(flags : U2) : Unit = { accessFlags = flags }

    /** Returns the currently set flags. */
    def getFlags : U2 = accessFlags

    /** Adds a field to the class, using the default flags and no attributes. */
    def stringToDescriptor(s: String) = s
    def addField(tpe: String, name: String): FieldHandler = {
        val accessFlags: U2 = defaultFieldAccessFlags
        val nameIndex: U2 = constantPool.addString(name)
        val descriptorIndex: U2 = constantPool.addString(stringToDescriptor(tpe))
        val inf = FieldInfo(accessFlags, nameIndex, descriptorIndex, Nil)
        fields = fields ::: (inf :: Nil)
        new FieldHandler(inf, constantPool)
    }

    /** Adds a method with arbitrarily many arguments, using the default flags and no attributes. */
    def addMethod(retTpe: String, name: String, args: String*)(code: Code): Unit = 
        addMethod(retTpe, name, args.toList)(code)

    def addMethod(retTpe: String, name: String, args: List[String])(code: Code): Unit = {

        val functionType = FunctionType(defaultMethodAccessFlags, retTpe, args*)

        val handler = new MethodHandler(name, functionType, codeNameIndex, constantPool)
        val info = handler(code)
        methods = methods ::: (info :: Nil)
    }

    /** Adds the main method */
    def addMainMethod(code: Code): Unit = {
        
        val accessFlags: U2 = Flag.METHOD_ACC_PUBLIC | Flag.METHOD_ACC_STATIC
        val functionType = FunctionType(accessFlags, "V", "[Ljava/lang/String;")

        val handler = new MethodHandler("main", functionType, codeNameIndex, constantPool)
        val info = handler(code)
        methods = methods ::: (info :: Nil)
    }

    /** Adds a constructor to the class. Constructor code should always start by invoking a constructor from the super class. */
    def addConstructor(args : String*)(code: Code): Unit = addConstructor(args.toList)(code)

    def addConstructor(args : List[String])(code: Code): Unit = {

        val accessFlags : U2 = Flag.METHOD_ACC_PUBLIC
        val functionType = FunctionType(accessFlags, "V", args*)
        
        val handler = new MethodHandler(constructorName, functionType, codeNameIndex, constantPool)
        val info = handler(code)
        methods = methods ::: (info :: Nil)
    }

    /** Adds a default constructor. */
    def addDefaultConstructor: Unit = {
        import ByteCode.{*, given}
        import Code.{*, given}

        val mh = addConstructor(Nil) {
            ALOAD_0 <<
            InvokeSpecial(Path(superClassName), constructorName, "()V") <<
            RETURN
        }
    }


    override def stream = State.modify { _
        << magic
        << minor
        << major
        << constantPool
        << accessFlags
        << thisClass
        << superClass
        << (interfaces.size: U2) << interfaces.reverse
        << (fields.size    : U2) << fields
        << (methods.size   : U2) << methods
        << (attributes.size: U2) << attributes
    }

    /** Writes the binary representation of this class file to a file. */
    def writeToFile(fileName : String) = {
        // The stream we'll ultimately use to write the class file data
        val byteStream = new ByteStream
        byteStream << this
        byteStream.writeToFile(fileName)
    }
}