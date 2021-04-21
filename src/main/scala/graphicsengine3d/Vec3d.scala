package graphicsengine3d

class Vec3d {
  var x: Double = 0.0
  var y: Double = 0.0
  var z: Double = 0.0
  def apply(x: Double, y: Double, z: Double): Unit = {
    this.x = x; this.y = y; this.z = z
  }
}