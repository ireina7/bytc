// The main entry of bytc
import scala.language.implicitConversions
import bytc.*
import bytc.given
import bytc.Type.*
import bytc.Type.given

@main def main: Unit = 
  
  println("Hello bytc!\n")
  // val tt = JVMType.from[Int]
  // println(tt)
  
  classFile.create() match
    case Left(err) => sys.error(err.msg)
    case Right(cf) => cf.writeToFile()
  
end main




def classFile = {
  import Code.*
  import ClassFile.Operation.{
    Define => DefineClass,
    *
  }

  DefineClass("HW")
    << DefaultConstructor
    << Main(helloWorld)
    << Method("I", "fact", "I")(fact << PrintCode)
}


def helloWorld: Code = {
  import Code.*
  
  val invokeFact = 
    Comment("Invoking fact(5)")
      << DefaultNew("HW")
      << load(5)
      << invokeVirtual("HW", "fact", "(I)I")
  
  val printStream = 
    GetStatic("java.lang.System", "out", "Ljava/io/PrintStream;")
  
  Comment("Hello world example")
    << printStream
    << load("Hello world!")
    << invokeVirtual("java.io.PrintStream", "println", "(Ljava/lang/String;)V")
    << printStream
    << invokeFact
    << invokeVirtual("java.io.PrintStream", "println", "(I)V")
    << Return
}

def fact: Code = {
  import Code.*
  
  val label = Tool.getFreshLabel()

  Code.define("fact(n: Int): Int") { List (

    Load(Var(1)),
    Load(1),
    If_ICmpGt(label),
    Load(1),
    Return,
    Label(label),
    Load(Var(1)),
    Load(Var(0)),
    Load(Var(1)), 
    Load(1),
    ISUB,
    InvokeVirtual("HW", "fact", "(I)I"),
    IMUL,
    Return,
  )}
  //Pass
}


