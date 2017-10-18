package pl.setblack.vide

import pl.setblack.vide.Code._
import pl.setblack.vide.walker.{JavaFile, JavaPackage}

import scala.util.Random

class Deployer(val root: JavaPackage) {

  val rnd = new Random("Radom".hashCode)
  val circleRadius = 100.0

  val antigravForce = 0.00002

  val packageIdealDist = 8.1
  val packageSpringK = 0.00009


  @volatile
  var forceMultiplier = 20.0

  @volatile
  var rootNode = initialDeploy(root)

  private def randomCoord(radius: Double): Coords = {
    val theta = rnd.nextDouble() * Math.PI
    val phi = rnd.nextDouble() * Math.PI * 2.0

    val x = radius * Math.sin(theta) * Math.cos(phi)
    val y = radius * Math.sin(theta) * Math.sin(phi)
    val z = radius * Math.cos(theta)
    Coords(x, y, z)
  }


  private def randomCoord: Coords = randomCoord(circleRadius)

  private def deploy(javas: Seq[JavaFile]): Seq[CodeNode] = {
    javas.map(jv => CodeNode(jv, randomCoord))
  }

  private def initialDeploy(rootPackage: JavaPackage) = {
    val initDist = 6.0
    new NodesPack(changeRoot(mapPackage(initDist, Coords(0, 0, 0), rootPackage, None)))
  }

  private def changeRoot(node : PackageNode) : PackageNode = {
    if ( node.subpackages.size == 1) {
      changeRoot( node.subpackages(0).copy(coords = Coords(0,0,0)))
    } else {
      node
    }
  }

  private def mapPackage(dist: Double, base: Coords, pack: JavaPackage, parent: Option[PackageNode]): PackageNode = {
    val newCoords = base.add(randomCoord(dist))
    val junctions = parent.map(p => List(Junction("parent", dist, 0.5, p.id))).getOrElse(Nil)

    val packageDefinition = PackageNode(pack.name,
      Nil,
      Nil,
      newCoords,
      junctions)

    packageDefinition.copy(
      subpackages = pack.subpackages.map(mapPackage(dist * 0.9, newCoords, _, Some(packageDefinition))),
      classes = pack.classes.map(mapClass(1.0 , newCoords, _, Some(packageDefinition)))
    )

  }

  private def mapClass(dist: Double, base: Coords, java: JavaFile, parent: Option[PackageNode]): CodeNode = {
    val newCoords = base.add(randomCoord(dist))
    val junctions = parent.map(p => List(Junction("parent", 0.5, 2.0, p.id))).getOrElse(Nil)
    CodeNode(java, newCoords, junctions)
  }


  def getRootNode = {
    rootNode
  }



  def relativeForces(weight: Double, src: Coords, source: PosNode, target: PosNode): _root_.pl.setblack.vide.Coords = {

    val diff = src.diff(target.coords)
    val distance = diff.module()
    val antigravityPos = {
      val force = -1.0 * weight * antigravForce*forceMultiplier / Math.max(distance, 0.0001)
      val push = diff.mult(force)
      src.add(push)
    }
    val existingJunction = source.junctions.find( junction => junction.targetId == target.id)



    val springed = existingJunction.map( junction => {
      val dist  =packageIdealDist*junction.distance
      val force = (distance - dist * dist) * packageSpringK * junction.forcMultiplier *  forceMultiplier
      val push = diff.mult(force)
      antigravityPos.add(push)
    }).getOrElse(antigravityPos)



    springed
  }

  def update(pack : PackageNode, all : NodesPack): PackageNode = {
    val weight = 1.0

    val newCoords = calcForces(pack, all)

    val subpackages = pack.subpackages.map( p => update(p, all))
    val classes = pack.classes.map( c => updateCode(c, all))
    pack.copy(coords = newCoords, subpackages =  subpackages, classes =classes)
  }

  def updateCode ( node : CodeNode, all : NodesPack) : CodeNode = {
    val weight = 0.5
    val newCoords = calcForces(node, all)
    node.copy(coords = newCoords)
  }


  def  calcForces( node : PosNode, all: NodesPack) : Coords = {
    val other = all.allNodes
    val newAntiGravCoord =  (node.coords /: other ) ( (src, target) => relativeForces( 1.0, src, node, target))

    //scale (move to center)
    val distToCenter = newAntiGravCoord.module()
    val scaled = if (distToCenter > circleRadius * circleRadius) {
      newAntiGravCoord.mult((circleRadius * circleRadius) / distToCenter)
    } else {
      newAntiGravCoord
    }


    scaled
  }

  //def updateJunctions( node : PosNode)


  def update: NodesPack = {

    val newRoot = update( rootNode.root, rootNode)
    rootNode = new NodesPack(newRoot)
    forceMultiplier = Math.max( forceMultiplier*0.99999, 1.0)
    rootNode
  }

}

case class Coords(x: Double, y: Double, z: Double) {
  def add(push: Coords): _root_.pl.setblack.vide.Coords = {
    Coords(x + push.x, y + push.y, z + push.z)
  }

  def diff(dst: Coords): Coords = {
    Coords(dst.x - x, dst.y - y, dst.z - z)
  }

  def mult(v: Double) = {
    Coords(x * v, y * v, z * v)
  }

  def module() = {
    x * x + y * y + z * z
  }

  def max(aMax: Double) = {
    this.copy(x = maxVal(x, aMax), y = maxVal(y, aMax), z = maxVal(z, aMax))
  }

  private def maxVal(aVal: Double, maximum: Double) = {
    if (Math.abs(aVal) > maximum) {
      maximum * Math.signum(aVal)
    } else {
      aVal
    }
  }

  def fix(): Coords = {
    this.copy(x = fixVal(x), y = fixVal(y), z = fixVal(z))
  }

  private def fixVal(a: Double): Double = {
    if (a == Double.NaN) {
      0
    } else {
      a
    }
  }

}


class NodesPack(val root : PackageNode) {

  val allNodes = root.getAll()
  val javaNodes = root.getAllJava()

  private val nodesById = calcById( allNodes)

  private def calcById(allNodes: List[PosNode]) :Map[String, PosNode] = {
    (allNodes map (_.id) zip allNodes).toMap
  }

  def getById( key : String) : Option[PosNode] = nodesById.get(key)

}


