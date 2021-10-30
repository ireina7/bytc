package bytc

import Type.*

/** A field handler is used to attach attributes to a field (currently, only
 * flags). <code>FieldHandler</code>s should not be created manually but
 * rather obtained directly when adding a field method to a
 * <code>ClassFile</code>. */
class FieldHandler (f: FieldInfo, cp: ConstantPool) {
  private val field: FieldInfo = f

  def setFlags(flags : U2) : Unit = { f.accessFlags = flags }
}