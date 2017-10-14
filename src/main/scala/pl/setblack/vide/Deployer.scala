package pl.setblack.vide

import pl.setblack.vide.walker.JavaFile
import Code._

import scala.util.Random

class Deployer(val javas: Seq[JavaFile]) {

  val rnd = new Random("Radom".hashCode)
  val circleRadius = 100.0

  val antigravForce = 0.0009

  val packageIdealDist = 3.1
  val packageSpringK = 0.000009

  @volatile
  var nodes = deploy(javas)

  private def randomCoord: Coords = {
    val theta = rnd.nextDouble() * Math.PI
    val phi = rnd.nextDouble() * Math.PI * 2.0

    val x = circleRadius * Math.sin(theta) * Math.cos(phi)
    val y = circleRadius * Math.sin(theta) * Math.sin(phi)
    val z = circleRadius * Math.cos(theta)
    Coords(x, y, z)
  }

  def deploy(javas: Seq[JavaFile]): Seq[CodeNode] = {
    javas.map(jv => CodeNode(jv, randomCoord))
  }


  def getNodes = {
    nodes
  }

  def update: Seq[CodeNode] = {
    val newNodes = nodes.map(nd1 => ((nd1) /: nodes) ((a, b) => update(a, b)))
    nodes = newNodes
    nodes
  }

  private def update(source: CodeNode, target: CodeNode): CodeNode = {
    //antigravity
    val diff = source.coords.diff(target.coords)
    val distance = diff.module()
    val antigravity = {
      val force = antigravForce / Math.max(distance, 0.00001)
      val push = diff.mult(force)
      source.coords.add(push)
    }

    //scale (move to center)
    val distToCenter = antigravity.module()
    val scaled = if (distToCenter > circleRadius * circleRadius) {
      antigravity.mult((circleRadius * circleRadius) / distToCenter)
    } else {
      antigravity
    }

    // package spring
    val packageSpringed = if (source.java.pack == target.java.pack && distance > 0) {
      val force = (distance - packageIdealDist * packageIdealDist) * packageSpringK
      val push = diff.mult(force)
      scaled.add(push)
    } else if (source.java.pack.indexOf(target.java.pack) >= 0 && distance > 0) {
      val force = (distance - packageIdealDist * packageIdealDist * 8) * packageSpringK
      val push = diff.mult(force / 10)
      scaled.add(push)
    } else {
      scaled
    }

    val res = CodeNode(source.java, packageSpringed.max(circleRadius))
    //println(s"${res.java.className} @ ${res.coords}")
    res

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



