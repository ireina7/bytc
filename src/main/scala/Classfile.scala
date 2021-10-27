package bytc

import scala.language.implicitConversions
import cats.*
import cats.data.State
import bytc.ClassFile.Operation
import Type.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*


trait JVMClass[F[_]: Monad, Code: Geass](using 
  ev0: ToStream[F, U2],
  ev1: ToStream[F, U4],
  ev2: ToStream[F, MethodInfo],
  ev3: ToStream[F, FieldInfo],
  ev4: ToStream[F, InterfaceInfo],
  ev5: ToStream[F, AttributeInfo],
  ev6: ToStream[F, ConstantPool],
  ev7: Monoid[F[Unit]],
) extends Streamable[F]:
  import Type.*
  import Defaults.*

  val magic: U4 = defaultMagic
  val version: Version = Version(defaultMajor, defaultMinor)
  val className: String
  val superName: Option[String]
  val superClassName: String = superName.getOrElse(defaultSuperClass)

  def thisClass   : U2
  def superClass  : U2
  def flags       : U2
  def constantPool: ConstantPool
  def methods     : List[MethodInfo   ]
  def fields      : List[FieldInfo    ]
  def interfaces  : List[InterfaceInfo]
  def attributes  : List[AttributeInfo]

  override def stream: F[Unit] = {
    given [A: [T] =>> ToStream[F, T]]: Conversion[A, F[Unit]] = _.toStream
    magic.toStream
    >> version.minor
    >> version.major
    >> constantPool
    >> flags
    >> thisClass
    >> superClass
    >> (interfaces.size: U2) >> interfaces
    >> (fields.size    : U2) >> fields
    >> (methods.size   : U2) >> methods
    >> (attributes.size: U2) >> attributes
  }
end JVMClass




class ClassFile(
  val className: String, 
  val superName: Option[String] = None
) extends JVMClass[ByteStreamState, Code]:

  import Type.*
  import Defaults.*
  

  def thisClass = _thisClass
  def superClass = _superClass
  def flags = accessFlags
  def constantPool = _constantPool
  def fields = fieldsBuffer
  def methods = methodsBuffer
  def interfaces = interfacesBuffer.reverse
  def attributes = attributesBuffer

  private var _constantPool = new ConstantPool()
  lazy val codeNameIndex: U2 = _constantPool.addString("Code") // Never change this since JVM defines this
  lazy val sourceFileNameIndex: U2 = _constantPool.addString("SourceFile")

  private var accessFlags: U2 = defaultClassAccessFlags
  private val _thisClass: U2 = _constantPool.addClass(_constantPool.addString(className))

  // private val superClassName: String = superName match {
  //   case None => "java/lang/Object"
  //   case Some(name) => name
  // }
  private var _superClass: U2 = _constantPool.addClass(_constantPool.addString(superClassName))

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
      InvokeSpecial(Path(superClassName), constructorName, "()V") <<
      RETURN
    }
  }


  // override def stream = State.modify { _
  //   << magic
  //   << version.minor
  //   << version.major
  //   << constantPool
  //   << accessFlags
  //   << thisClass
  //   << superClass
  //   << (interfaces.size: U2) << interfaces.reverse
  //   << (fields.size    : U2) << fields
  //   << (methods.size   : U2) << methods
  //   << (attributes.size: U2) << attributes
  // }

  /** Writes the binary representation of this class file to a file. */
  def writeToFile(fileName : String) = {
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