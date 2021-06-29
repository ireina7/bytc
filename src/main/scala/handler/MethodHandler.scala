package bytc

import Type._

/** A method handler is used to attach attributes to a method. In particular,
 * it can return an associated `CodeHandler` which can be used to
 * specify a method's body. 
 * 
 * `MethodHandler`s should not be created
 * manually but rather obtained directly when adding a method to a
 * `ClassFile`. */
class MethodHandler(
    name: String, 
    functionType: FunctionType,
    codeIndex: U2,
    constantPool: ConstantPool, 
) {
    
    private var ch : Option[CodeHandler] = None
    val accessFlags: U2 = functionType.accessFlags
    val paramTypes: String = functionType.paramType.mkString("")
    val retType: String = functionType.retType

    val functionSig = constantPool.functionTypeToSignature(name, functionType)

    val codeInfo = new CodeAttributeInfo(codeIndex)

    def codeHandler : CodeHandler = {
        if ch.isEmpty then 
            ch = Some(new CodeHandler(codeInfo, constantPool, functionType))
        
        ch.get
    }

    def apply(src: Code): Result[MethodInfo] = 
        for (codeInfo <- code(src)) yield new MethodInfo(functionSig, List(codeInfo))

    def code(src: Code): Result[CodeAttributeInfo] =
        src(this.codeHandler).freeze()

    /*
    def setFlags(flags: U2): Unit = {
        if ch.isDefined then 
            if methodInfo.isStatic != ((flags & Flag.METHOD_ACC_STATIC) != 0) then
                sys.error("Cannot change the `static` attribute of a method after its CodeHandler has been issued.")

        methodInfo.accessFlags = flags
    }
    */

}