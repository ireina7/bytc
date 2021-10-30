package bytc

import scala.language.implicitConversions
import cats.*
import cats.data.State
// import bytc.ClassFile.Operation
import Type.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import Defaults.*



trait IsJVMClass[F[_]: Monad, JVMClass]:
  extension (jvmClass: JVMClass)
    def magic       : F[U4] = defaultMagic.pure
    def version     : F[Version]
    def className   : F[String]
    def superName   : F[Option[String]]

    def thisClass   : F[U2]
    def superClass  : F[U2]
    def flags       : F[U2]
    def constantPool: F[ConstantPool]
    def methods     : F[List[MethodInfo   ]]
    def fields      : F[List[FieldInfo    ]]
    def interfaces  : F[List[InterfaceInfo]]
    def attributes  : F[List[AttributeInfo]]
    def superClassName: F[String] = superName.map(_.getOrElse(defaultSuperClass))
end IsJVMClass


object IsJVMClass {

  given IsJVMClass[Id, ClassFile] with {
    extension (o: ClassFile)
      def version = o.getVersion
      def className = o.getClassName
      def superName = o.getSuperName

      def thisClass = o.getThisClass
      def superClass = o.getSuperClass
      def flags = o.getFlags
      def constantPool = o.getConstantPool
      def methods = o.getMethods
      def fields = o.getFields
      def interfaces = o.getInterfaces
      def attributes = o.getAttributes
  }

  given [F[_]: Monad, JVMClass](using idEv: IsJVMClass[Id, JVMClass])
    : IsJVMClass[F, JVMClass] with {
    extension (o: JVMClass)
      def version = idEv.version(o).pure
      def className = idEv.className(o).pure
      def superName = idEv.superName(o).pure

      def thisClass = idEv.thisClass(o).pure
      def superClass = idEv.superClass(o).pure
      def flags = idEv.flags(o).pure
      def constantPool = idEv.constantPool(o).pure
      def methods = idEv.methods(o).pure
      def fields = idEv.fields(o).pure
      def interfaces = idEv.interfaces(o).pure
      def attributes = idEv.attributes(o).pure
  }
}




given [F[_]: Monad, JVMClass](using 
  IsJVMClass[F, JVMClass],
  ToStream[F, U2],
  ToStream[F, U4],
  ToStream[F, MethodInfo],
  ToStream[F, FieldInfo],
  ToStream[F, InterfaceInfo],
  ToStream[F, AttributeInfo],
  ToStream[F, ConstantPool],
  Monoid[F[Unit]],
): ToStream[F, JVMClass] with {
  extension (x: JVMClass) def toStream: F[Unit] = {
    given [A: [T] =>> ToStream[F, T]]: Conversion[A, F[Unit]] = _.toStream
    for {
      magic        <- x.magic
      version      <- x.version
      constantPool <- x.constantPool
      flags        <- x.flags
      thisClass    <- x.thisClass
      superClass   <- x.superClass
      interfaces   <- x.interfaces
      fields       <- x.fields
      methods      <- x.methods
      attributes   <- x.attributes
      stream       <- magic.toStream
                        >> version.minor
                        >> version.major
                        >> constantPool
                        >> flags
                        >> thisClass
                        >> superClass
                        >> (interfaces.size: U2) >> interfaces
                        >> (fields    .size: U2) >> fields
                        >> (methods   .size: U2) >> methods
                        >> (attributes.size: U2) >> attributes
    } yield stream
  }
}






/**
 * An implementation of JVM class file
*/
class ClassFile(
  val className: String, 
  val superName: Option[String] = None
):

  import Type.*
  import Defaults.*
  import IsJVMClass.given
  
  def getClassName = className
  def getSuperName = superName
  def getVersion = Version(defaultMajor, defaultMinor)
  def getThisClass = _thisClass
  def getSuperClass = _superClass
  // def getFlags = accessFlags
  def getConstantPool = _constantPool
  def getFields = fieldsBuffer
  def getMethods = methodsBuffer
  def getInterfaces = interfacesBuffer.reverse
  def getAttributes = attributesBuffer

  private var _constantPool = new ConstantPool()
  lazy val codeNameIndex: U2 = _constantPool.addString("Code") // Never change this since JVM defines this
  lazy val sourceFileNameIndex: U2 = _constantPool.addString("SourceFile")

  private var accessFlags: U2 = defaultClassAccessFlags
  private val _thisClass: U2 = _constantPool.addClass(_constantPool.addString(className))

  // private val superClassName: String = superName match {
  //   case None => "java/lang/Object"
  //   case Some(name) => name
  // }
  private var _superClass: U2 = _constantPool.addClass(_constantPool.addString(this.superClassName))

  private var fieldsBuffer     : List[FieldInfo]     = Nil
  private var methodsBuffer    : List[MethodInfo]    = Nil
  private var interfacesBuffer : List[InterfaceInfo] = Nil
  private var attributesBuffer : List[AttributeInfo] = Nil

  
  def addInterface(name: String) = {
    val nameIndex = _constantPool.addClass(_constantPool.addString(name))
    interfacesBuffer = InterfaceInfo(name, nameIndex) :: interfacesBuffer
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
    val nameIndex: U2 = _constantPool.addString(name)
    val descriptorIndex: U2 = _constantPool.addString(stringToDescriptor(tpe))
    val inf = FieldInfo(accessFlags, nameIndex, descriptorIndex, Nil)
    fieldsBuffer = fieldsBuffer ::: (inf :: Nil)
    new FieldHandler(inf, _constantPool)
  }

  /** Adds a method with arbitrarily many arguments, using the default flags and no attributes. */
  def addMethod(retTpe: String, name: String, args: String*)(code: Code): Result[Unit] = 
    addMethod(defaultMethodAccessFlags, retTpe, name, args.toList)(code)

  def addMethod(accessFlags: U2, retTpe: String, name: String, args: List[String])(code: Code): Result[Unit] = {

    val functionType = FunctionType(accessFlags, retTpe, args*)

    val handler = new MethodHandler(name, functionType, codeNameIndex, _constantPool)
    for {
      info <- handler(code)
    } yield {
      methodsBuffer = methodsBuffer ::: (info :: Nil)
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
    import Code.*
    
    addConstructor(Nil) {
      ALOAD_0 <<
      InvokeSpecial(Path(this.superClassName), constructorName, "()V") <<
      RETURN
    }
  }

  /** Writes the binary representation of this class file to a file. */
  def writeToFile(fileName: String = s"$className.class") = {
    // The stream we'll ultimately use to write the class file data
    val byteStream = new ByteStream
    byteStream << this
    byteStream.writeToFile(fileName)
  }
end ClassFile



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