package bytc

import Type.*


object Tool:

  def version(i: Int): Version = ???

  inline def transformLiteralPath(path: String): String = ???

  def transformPath(path: String): String = 
    path.trim.split("\\.").mkString("/")

  private var labelCount = 0
  // I know, yes, this is evil and dirty, but this works, who care fp?
  def getFreshLabel(prefix: String = "label"): String = 
    labelCount += 1
    s"$prefix $labelCount"

end Tool


/**
 * Default tool implicit conversions inside `bytc` package
*/
given Conversion[String, Path] with 
  def apply(s: String) = Path(Tool.transformPath(s))
