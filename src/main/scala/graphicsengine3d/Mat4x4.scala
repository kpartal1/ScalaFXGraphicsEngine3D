package graphicsengine3d

class Mat4x4 {
  var m: Array[Array[Double]] = Array.fill(4)(Array.fill(4)(0))

  def makeIdentity: Mat4x4 = {
    val matrix: Mat4x4 = new Mat4x4
    matrix.m(0)(0) = 1.0
    matrix.m(1)(1) = 1.0
    matrix.m(2)(2) = 1.0
    matrix.m(3)(3) = 1.0
    matrix
  }

  def makeRotX(fAngleRad: Double): Mat4x4 = {
    val matrix: Mat4x4 = new Mat4x4
    matrix.m(0)(0) = 1.0
    matrix.m(1)(1) = math.cos(fAngleRad)
    matrix.m(1)(2) = math.sin(fAngleRad)
    matrix.m(2)(1) = -math.sin(fAngleRad)
    matrix.m(2)(2) = math.cos(fAngleRad)
    matrix.m(3)(3) = 1.0
    matrix
  }

  def makeRotY(fAngleRad: Double): Mat4x4 = {
    val matrix: Mat4x4 = new Mat4x4
    matrix.m(0)(0) = math.cos(fAngleRad)
    matrix.m(0)(2) = math.sin(fAngleRad)
    matrix.m(2)(0) = -math.sin(fAngleRad)
    matrix.m(1)(1) = 1.0
    matrix.m(2)(2) = math.cos(fAngleRad)
    matrix.m(3)(3) = 1.0
    matrix
  }

  def makeRotZ(fAngleRad: Double): Mat4x4 = {
    val matrix: Mat4x4 = new Mat4x4
    matrix.m(0)(0) = math.cos(fAngleRad)
    matrix.m(0)(1) = math.sin(fAngleRad)
    matrix.m(1)(0) = -math.sin(fAngleRad)
    matrix.m(1)(1) = math.cos(fAngleRad)
    matrix.m(2)(2) = 1.0
    matrix.m(3)(3) = 1.0
    matrix
  }

  def makeTranslation(x: Double, y: Double, z: Double): Mat4x4 = {
    val matrix: Mat4x4 = new Mat4x4
    matrix.m(0)(0) = 1.0
    matrix.m(1)(1) = 1.0
    matrix.m(2)(2) = 1.0
    matrix.m(3)(3) = 1.0
    matrix.m(3)(0) = x
    matrix.m(3)(1) = y
    matrix.m(3)(2) = z
    matrix
  }

  def makeProjection(fFovDegrees: Double, fAspectRatio: Double, fNear: Double, fFar: Double) : Mat4x4 = {
    val fFovRad: Double = 1.0 / math.tan(fFovDegrees * 0.5 / 180.0 * math.Pi)
    val matrix: Mat4x4 = new Mat4x4
    matrix.m(0)(0) = fAspectRatio * fFovRad
    matrix.m(1)(1) = fFovRad
    matrix.m(2)(2) = fFar / (fFar - fNear)
    matrix.m(3)(2) = (-fFar * fNear) / (fFar - fNear)
    matrix.m(2)(3) = 1.0
    matrix.m(3)(3) = 0.0
    matrix
  }

  def multiplyMatrix(m1: Mat4x4, m2: Mat4x4): Mat4x4 = {
    val matrix: Mat4x4 = new Mat4x4
    for(c <- 0 until 4) {
      for(r <- 0 until 4) {
        matrix.m(r)(c) = m1.m(r)(0) * m2.m(0)(c) + m1.m(r)(1) * m2.m(1)(c) + m1.m(r)(2) * m2.m(2)(c) + m1.m(r)(3) * m2.m(3)(c)
      }
    }
    matrix
  }

  def multiplyByMatrix(m: Mat4x4): Mat4x4 = {
    val matrix: Mat4x4 = new Mat4x4
    for(c <- 0 until 4) {
      for(r <- 0 until 4) {
        matrix.m(r)(c) = this.m(r)(0) * m.m(0)(c) + this.m(r)(1) * m.m(1)(c) + this.m(r)(2) * m.m(2)(c) + this.m(r)(3) * m.m(3)(c)
      }
    }
    matrix
  }

  def pointAtMatrix(pos: Vec3d, target: Vec3d, up: Vec3d): Mat4x4 = {
    // Calculate new forward direction
    val newForward: Vec3d = target - pos
    val normalizednewForward = newForward.normalize

    // Calculate new up direction
    val a: Vec3d = normalizednewForward * (up.dotProduct(normalizednewForward))
    val newUp: Vec3d = up - a
    val normalizednewUp = newUp.normalize

    // New right direction
    val newRight: Vec3d = normalizednewUp.crossProduct(normalizednewForward)

    // Construct dimensioning and translation matrix
    val matrix: Mat4x4 = new Mat4x4
    matrix.m(0)(0) = newRight.x;	            matrix.m(0)(1) = newRight.y;	            matrix.m(0)(2) = newRight.z;	            matrix.m(0)(3) = 0.0
		matrix.m(1)(0) = normalizednewUp.x;		    matrix.m(1)(1) = normalizednewUp.y;		    matrix.m(1)(2) = normalizednewUp.z;		    matrix.m(1)(3) = 0.0
		matrix.m(2)(0) = normalizednewForward.x;  matrix.m(2)(1) = normalizednewForward.y;	matrix.m(2)(2) = normalizednewForward.z;	matrix.m(2)(3) = 0.0
		matrix.m(3)(0) = pos.x;			              matrix.m(3)(1) = pos.y;			              matrix.m(3)(2) = pos.z;			              matrix.m(3)(3) = 1.0
    matrix
  }

  // Only for rotation/translation matrices
  def quickInverse(m: Mat4x4): Mat4x4 = {
    val matrix: Mat4x4 = new Mat4x4
    matrix.m(0)(0) = m.m(0)(0); matrix.m(0)(1) = m.m(1)(0); matrix.m(0)(2) = m.m(2)(0); matrix.m(0)(3) = 0.0
		matrix.m(1)(0) = m.m(0)(1); matrix.m(1)(1) = m.m(1)(1); matrix.m(1)(2) = m.m(2)(1); matrix.m(1)(3) = 0.0
		matrix.m(2)(0) = m.m(0)(2); matrix.m(2)(1) = m.m(1)(2); matrix.m(2)(2) = m.m(2)(2); matrix.m(2)(3) = 0.0
		matrix.m(3)(0) = -(m.m(3)(0) * matrix.m(0)(0) + m.m(3)(1) * matrix.m(1)(0) + m.m(3)(2) * matrix.m(2)(0))
		matrix.m(3)(1) = -(m.m(3)(0) * matrix.m(0)(1) + m.m(3)(1) * matrix.m(1)(1) + m.m(3)(2) * matrix.m(2)(1))
		matrix.m(3)(2) = -(m.m(3)(0) * matrix.m(0)(2) + m.m(3)(1) * matrix.m(1)(2) + m.m(3)(2) * matrix.m(2)(2))
		matrix.m(3)(3) = 1.0
    matrix
  }
}