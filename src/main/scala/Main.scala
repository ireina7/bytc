// The main entry of bytc
import scala.language.implicitConversions
import bytc.*
import bytc.Type.*
import bytc.given

@main def main: Unit = 
  import Code.*
  println("Hello bytc!")

  val classfile = ClassFile("HW")
  classfile.addDefaultConstructor
  classfile.addMainMethod(helloWorld)
  classfile.addMethod("I", "fact", "I")(fact << PrintCode)

  classfile.writeToFile("HW.class")

end main



def helloWorld: Code = {
  import Code.*
  

  val cs = RawByte(1) << ALOAD_0 << ALoad(7) << RETURN

  val invokeFact = 
    Comment("Invoking fact(5)")
      << DefaultNew("HW")
      << Load(5)
      << InvokeVirtual("HW", "fact", "(I)I")
  
  val printStream = 
    GetStatic("java.lang.System", "out", "Ljava/io/PrintStream;")
  
  Comment("Hello world example")
    << printStream
    << Load("Hello world!")
    << InvokeVirtual("java.io.PrintStream", "println", "(Ljava/lang/String;)V")
    << printStream
    << invokeFact
    << InvokeVirtual("java.io.PrintStream", "println", "(I)V")
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


