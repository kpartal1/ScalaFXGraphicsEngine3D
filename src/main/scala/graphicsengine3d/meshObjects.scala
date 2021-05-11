package graphicsengine3d

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object MeshObjects {
  def loadObjectFromFile(fileName: String, bHasTexture: Boolean = false): Mesh = {
    val tri: ArrayBuffer[Triangle] = ArrayBuffer[Triangle]()
    val verts: ArrayBuffer[Vec3d] = ArrayBuffer[Vec3d]()
    val texs: ArrayBuffer[Vec2d] = ArrayBuffer[Vec2d]()
    val src: Source = Source.fromFile(fileName)
    for(lines <- src.getLines) {
      val line: Array[String] = lines.split(" ")

      if(line(0) == "v") {
          val v: Vec3d = new Vec3d
          v.x = line(1).toDouble
          v.y = line(2).toDouble
          v.z = line(3).toDouble
          verts += v
      }
      
      if (line(0) == "vt") {
          val v: Vec2d = new Vec2d
          v.u = line(1).toDouble
          v.v = line(2).toDouble
          texs += v
      }

      if(!bHasTexture) {
        if (line(0) == "f") {
          val t: Triangle = new Triangle
          t.apply(verts(line(1).toInt - 1), verts(line(2).toInt - 1), verts(line(3).toInt - 1))
          tri += t
        }
      } else {
        if(line(0) == "f") {
          val pLine: Array[String] = line.drop(1)
          val t: Triangle = new Triangle
          val tokens: Array[String] = Array.fill(6)("")
          var nTokenCount: Int = 0

          for(i <- pLine) {
            val nLine: Array[String] = i.split("/")
            tokens(nTokenCount) = nLine(0)
            tokens(nTokenCount + 1) = nLine(1)
            nTokenCount += 2
          }

          t.apply(verts(tokens(0).toInt - 1), verts(tokens(2).toInt - 1), verts(tokens(4).toInt - 1))
          t.applyTex(texs(tokens(1).toInt - 1), texs(tokens(3).toInt - 1), texs(tokens(5).toInt - 1))

          tri += t
        }
      }
    }
    src.close()
    val mesh: Mesh = new Mesh
    mesh.tris = tri
    mesh
  }
}