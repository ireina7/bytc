package bytc


object Defaults {
    import Type._
    import Flag._

    inline val defaultMagic = 0xCAFEBABE
    inline val defaultMinor = 0
    inline val defaultMajor = 49 // J2SE 6.0=50, J2SE 5.0=49, JDK 1.4=48, JDK 1.3=47, JDK 1.2=46, JDK 1.1=45
    inline val defaultSuperClass = "java/lang/Object"

    val defaultClassAccessFlags : U2 = CLASS_ACC_PUBLIC | CLASS_ACC_SUPER
    val defaultMethodAccessFlags: U2 = METHOD_ACC_PUBLIC
    val defaultFieldAccessFlags : U2 = FIELD_ACC_PROTECTED

    inline val constructorName = "<init>"
    inline val constructorSig = "()V"
}