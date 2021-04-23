package graphicsengine3d

class Vec3d(var x: Double = 0.0, var y: Double = 0.0, var z: Double = 0.0) {
  var w: Double = 1.0
  
  def multiplyMatrixVector(m: Mat4x4, i: Vec3d): Vec3d = {
    this.x = i.x * m.m(0)(0) + i.y * m.m(1)(0) + i.z * m.m(2)(0) + i.w * m.m(3)(0)
    this.y = i.x * m.m(0)(1) + i.y * m.m(1)(1) + i.z * m.m(2)(1) + i.w * m.m(3)(1)
    this.z = i.x * m.m(0)(2) + i.y * m.m(1)(2) + i.z * m.m(2)(2) + i.w * m.m(3)(2)
    this.w = i.x * m.m(0)(3) + i.y * m.m(1)(3) + i.z * m.m(2)(3) + i.w * m.m(3)(3)
    this
  }

  def + (v: Vec3d): Vec3d = {
    val vector: Vec3d = new Vec3d
    vector.x = this.x + v.x
    vector.y = this.y + v.y
    vector.z = this.z + v.z
    vector.w = this.w + v.w
    vector
  }

  def - (v: Vec3d): Vec3d = {
    val vector: Vec3d = new Vec3d
    vector.x = this.x - v.x
    vector.y = this.y - v.y
    vector.z = this.z - v.z
    vector
  }  

  def * (k: Double): Vec3d = {
    val vector: Vec3d = new Vec3d
    vector.x = this.x * k
    vector.y = this.y * k
    vector.z = this.z * k
    vector
  }
  
  def / (k: Double): Vec3d = {
    val vector: Vec3d = new Vec3d
    vector.x = this.x / k
    vector.y = this.y / k
    vector.z = this.z / k
    vector
  }

  def dotProduct(v: Vec3d): Double = {
    this.x * v.x + this.y * v.y + this.z * v.z
  }

  def length: Double = {
    math.sqrt(this.dotProduct(this))
  }

  def normalize: Vec3d = {
    val l: Double = this.length
    this.x /= l; this.y /= l; this.z /= l
    this
  }

  def crossProduct(v: Vec3d): Vec3d = {
    var ret: Vec3d = new Vec3d
    ret.x = this.y * v.z - this.z * v.y
    ret.y = this.z * v.x - this.x * v.z
    ret.z = this.x * v.y - this.y * v.x
    ret
  }
}
