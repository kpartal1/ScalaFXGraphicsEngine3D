package graphicsengine3d

class Triangle {
  var p: Array[Vec3d] = Array.fill(3){new Vec3d}
  var col: Double = 0.0
  def apply(vec1: Vec3d, vec2: Vec3d, vec3: Vec3d): Unit = {
    this.p(0) = vec1; this.p(1) = vec2; this.p(2) = vec3
  }
  def triSort(t1: Triangle): Boolean = {
    val z1: Double = (this.p(0).z + this.p(1).z + this.p(2).z) / 3.0
    val z2: Double = (t1.p(0).z + t1.p(1).z + t1.p(2).z) / 3.0
    z1 > z2
  }
}