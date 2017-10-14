package pl.setblack.vide

import pl.setblack.vide.walker.JavaFile
import play.api.libs.json.{JsValue, Json}

object Code {
  implicit val javaWriter =  Json.writes[JavaFile]
  implicit val coordsWriter =  Json.writes[Coords]
  implicit val codeNodeWriter =  Json.writes[CodeNode]


  implicit val packageWriter =  Json.writes[Package]

  case class Package( name : Seq[String], subpackages : List[Package],  classes : List[CodeNode], coords : Coords) {
    def toJson : JsValue = packageWriter.writes(this)
  }

  case class CodeNode(java: JavaFile, coords: Coords) {
    def toJson : JsValue = codeNodeWriter.writes(this)
  }
}
