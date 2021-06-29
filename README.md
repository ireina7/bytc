# Bytc - bytc your code
Purely functional byte code and class file generator written in Scala3

```
    ____        __      
   / __ )__  __/ /______
  / __  / / / / __/ ___/
 / /_/ / /_/ / /_/ /__  
/_____/\__, /\__/\___/  
      /____/            
```


## Designs
- The core class `Code` and `ClassFile.Operation` are purely composable, which implement `Monoid`.
- No side effect until we have to write file (IO).
- Computation description and interpreter implementation properly seperated
  (Free monad is not necessary with Scala's implicits).


## Usage

### Useful codes
- `Pass` serves as the empty code
- `Load` Intelligently load various kinds of data types
- `Store` Intelligently store various kinds of data types
- `Return` Intelligently return with return type
- `Inspect` Inspect intermediate code state
- `PrintCode` Print codes
- `NewCode` Customize code as you like
- `New` new instances
- `InvokeVirtual` invoke methods
- ...

### Useful classfile operations
- `Define` defines a new class
- `Main` adds main method
- `Method` adds method
- `Constructor` adds constructor
- `DefaultConstructor` adds default constructor
- ...


## Examples

### Necessary imports
```scala
import bytc.*
import bytc.given
import Type.*
```

### Hello world
```scala
@main def main: Unit = 
  import Code.*

  import ClassFile.Operation.*
  val `class` = 
    Define("HelloWorld")
      << DefaultConstructor
      << Main(helloWorld)

  `class`.create() match
    case Left(err) => sys.error(err.msg)
    case Right(cf) => cf.writeToFile("HelloWorld.class")
end main

def helloWorld: Code = {
  import Code.*
  
  Comment("Hello world example")
    << GetStatic("java.lang.System", "out", "Ljava/io/PrintStream;")
    << Load("Hello world!")
    << InvokeVirtual("java.io.PrintStream", "println", "(Ljava/lang/String;)V")
    << Return
}
```

### Factorial
```scala
def fact: Code = {
  import Code.*
  
  val label = Tool.getFreshLabel()

  Comment("Define fact(n: Int): Int") 
    << Load(Var(1)) 
    << Load(1)
    << If_ICmpGt(label) 
    << Load(1) 
    << Return
    << Label(label)
    << Load(Var(1)) 
    << Load(Var(0))
    << Load(Var(1)) 
    << Load(1) 
    << ISUB 
    << InvokeVirtual("HelloWorld", "fact", "(I)I")
    << IMUL 
    << Return
}

```