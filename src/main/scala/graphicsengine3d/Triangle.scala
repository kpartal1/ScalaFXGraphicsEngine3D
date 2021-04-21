package graphicsengine3d

class Triangle {
  var p: Array[Vec3d] = Array.fill(3){new Vec3d}
  var col: Double = 0.0
  def apply(vec1: Vec3d, vec2: Vec3d, vec3: Vec3d): Unit = {
    this.p(0) = vec1; this.p(1) = vec2; this.p(2) = vec3
  }
}