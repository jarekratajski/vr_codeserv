package pl.setblack.vide

import pl.setblack.vide.walker.{JavaFile, Violation}
import play.api.libs.json.{JsValue, Json, OWrites, Writes}

object Code {
  implicit val violationWriter =  Json.writes[Violation]
  implicit val javaWriter =  Json.writes[JavaFile]

  //implicit val posNodeWriter =  Json.writes[PosNode]
  implicit val junctionWrite:Writes[Junction] = (o: Junction) => {
    o.toJSON
  }

  implicit val coordsWriter =  Json.writes[Coords]
  implicit val codeNodeWriter =  Json.writes[CodeNode]


  implicit val packageWriter =  Json.writes[PackageNode]

  sealed trait PosNode {
    def toJSON : JsValue
    def coords : Coords
    def id  : String
    def junctions : List[Junction]

  }

  case class PackageNode(name : Seq[String], subpackages : List[PackageNode], classes : List[CodeNode], coords : Coords, junctions: List[Junction] = Nil)  extends PosNode {
    def getAll():List[PosNode] = {
      this +: (subpackages.flatMap(p => p.getAll()) ++ classes)
    }

    def getAllJava():List[CodeNode] = {
      (subpackages.flatMap(p => p.getAllJava()) ++ classes)
    }

    def toJSON : JsValue = packageWriter.writes(this)
    def id : String = name.mkString(".")
  }

  case class CodeNode(java: JavaFile, coords: Coords,  junctions: List[Junction] = Nil) extends PosNode {
    def toJSON : JsValue = codeNodeWriter.writes(this)
    def id : String = java.pack + "."+ java.className
  }


  case class Junction ( name : String, distance : Double, forcMultiplier: Double, targetId : String) {
    def toJSON : JsValue = {
      implicit val trWriter =  Json.writes[Junction]
      trWriter.writes(this)
    }
  }

}
