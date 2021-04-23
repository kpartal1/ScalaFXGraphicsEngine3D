package graphicsengine3d

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object MeshObjects {
  def loadObjectFromFile(fileName: String): Mesh = {
    val tri: ArrayBuffer[Triangle] = ArrayBuffer[Triangle]()
    val tmp: ArrayBuffer[Vec3d] = ArrayBuffer[Vec3d]()
    val src: Source = Source.fromFile(fileName)
    for(lines <- src.getLines) {
      val line: Array[String] = lines.split(" ")
      if(line(0) == "v") {
        val v: Vec3d = new Vec3d
        v.x = line(1).toDouble
        v.y = line(2).toDouble
        v.z = line(3).toDouble
        tmp += v
      } else if (line(0) == "f") {
        val t: Triangle = new Triangle
        t.apply(tmp(line(1).toInt - 1), tmp(line(2).toInt - 1), tmp(line(3).toInt - 1))
        tri += t
      }
    }
    src.close()
    val mesh: Mesh = new Mesh
    mesh.tris = tri
    mesh
  }
}