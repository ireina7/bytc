package bytc

import scala.language.implicitConversions
import cats.data.State
import bytc.ClassFile.Operation


class ClassFile(val className: String, superName: Option[String] = None) extends Streamable {
    import Type.*
    import Defaults.*
    

    val magic: U4 = defaultMagic
    val version = Version(defaultMajor, defaultMinor)

    private var constantPool = new ConstantPool()
    lazy val codeNameIndex: U2 = constantPool.addString("Code") // Never change this since JVM defines this
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
    def addMethod(retTpe: String, name: String, args: String*)(code: Code): Result[Unit] = 
        addMethod(defaultMethodAccessFlags, retTpe, name, args.toList)(code)

    def addMethod(accessFlags: U2, retTpe: String, name: String, args: List[String])(code: Code): Result[Unit] = {

        val functionType = FunctionType(accessFlags, retTpe, args*)

        val handler = new MethodHandler(name, functionType, codeNameIndex, constantPool)
        for {
            info <- handler(code)
        } yield {
            methods = methods ::: (info :: Nil)
        }
    }

    /** Adds the main method */
    def addMainMethod(code: Code): Result[Unit] = {
        
        val accessFlags: U2 = Flag.METHOD_ACC_PUBLIC | Flag.METHOD_ACC_STATIC
        addMethod(accessFlags, "V", "main", "[Ljava/lang/String;" :: Nil)(code)
    }

    /** Adds a constructor to the class. Constructor code should always start by invoking a constructor from the super class. */
    def addConstructor(args : String*)(code: Code): Result[Unit] = addConstructor(args.toList)(code)

    def addConstructor(args : List[String])(code: Code): Result[Unit] = {

        val accessFlags : U2 = Flag.METHOD_ACC_PUBLIC
        addMethod(accessFlags, "V", constructorName, args)(code)
    }

    /** Adds a default constructor. */
    def addDefaultConstructor: Result[Unit] = {
        import ByteCode.{*, given}
        import Code.{*, given}

        addConstructor(Nil) {
            ALOAD_0 <<
            InvokeSpecial(Path(superClassName), constructorName, "()V") <<
            RETURN
        }
    }


    override def stream = State.modify { _
        << magic
        << version.minor
        << version.major
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



object ClassFile:
    import cats.data.State

    class Operation(val state: State[ClassFile, Result[ClassFile]]):
        def <<(next: Operation) = new Operation(this.state.flatMap(_ => next.state))
        def run(cf: ClassFile): Result[ClassFile] = this.state.run(cf).value._2
        def create(): Result[ClassFile] = this.run(new ClassFile("Null"))

    object Operation:

        inline def newState[S, A](f: S => (S, A)): State[S, A] = State[S, A](f)
        //inline def modify[S, A](f: S => Result[])

        case class Define(name: String) extends Operation(newState { cf =>
            val newCf = new ClassFile(name)
            (newCf, Right(newCf))
        })

        case class Main(code: Code) extends Operation(newState { cf =>
            val res = for(_ <- cf.addMainMethod(code)) yield cf
            (cf, res)
        })

        case class Constructor(args: String*)(code: Code) extends Operation(newState { cf =>
            val res = for(_ <- cf.addConstructor(args*)(code)) yield cf
            (cf, res)
        })

        case object DefaultConstructor extends Operation(newState { cf =>
            val res = for(_ <- cf.addDefaultConstructor) yield cf
            (cf, res)
        })

        case class Method(retTpe: String, name: String, args: String*)(code: Code) extends Operation(newState { cf =>
            val res = for(_ <- cf.addMethod(retTpe, name, args*)(code)) yield cf
            (cf, res)
        })

        case class Inspect(f: ClassFile => Unit) extends Operation(newState { cf => 
            f(cf)
            (cf, Right(cf)) 
        })

    end Operation

end ClassFile