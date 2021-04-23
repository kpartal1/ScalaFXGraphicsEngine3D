package graphicsengine3d

import scalafx.scene.paint.Color

class Triangle {
  var p: Array[Vec3d] = Array.fill(3){new Vec3d}
  var brightness = 1.0
  var col: Color = Color.WHITE
  def apply(vec1: Vec3d, vec2: Vec3d, vec3: Vec3d): Unit = {
    this.p(0) = vec1; this.p(1) = vec2; this.p(2) = vec3
  }
  
  def triSort(t: Triangle): Boolean = {
    val z1: Double = (this.p(0).z + this.p(1).z + this.p(2).z) / 3.0
    val z2: Double = (t.p(0).z + t.p(1).z + t.p(2).z) / 3.0
    z1 > z2
  }
}