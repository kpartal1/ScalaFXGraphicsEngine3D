package graphicsengine3d

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.canvas.Canvas
import scalafx.scene.Scene
import scalafx.animation.AnimationTimer
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scalafx.scene.input.KeyEvent
import scalafx.scene.input.KeyCode

object Main extends JFXApp {

  private var upHeld = false
  private var downHeld = false
  private var leftHeld = false
  private var rightHeld = false
  private var aHeld = false
  private var wHeld = false
  private var sHeld = false
  private var dHeld = false

  def triSort(t1: Triangle, t2: Triangle): Boolean = {
    val z1: Double = (t1.p(0).z + t1.p(1).z + t1.p(2).z) / 3.0
    val z2: Double = (t2.p(0).z + t2.p(1).z + t2.p(2).z) / 3.0
    z1 > z2
  }

  private def multiplyMatrixVector(m: Mat4x4, i: Vec3d): Vec3d = {
    val v: Vec3d = new Vec3d
    v.x = i.x * m.m(0)(0) + i.y * m.m(1)(0) + i.z * m.m(2)(0) + i.w * m.m(3)(0)
    v.y = i.x * m.m(0)(1) + i.y * m.m(1)(1) + i.z * m.m(2)(1) + i.w * m.m(3)(1)
    v.z = i.x * m.m(0)(2) + i.y * m.m(1)(2) + i.z * m.m(2)(2) + i.w * m.m(3)(2)
    v.w = i.x * m.m(0)(3) + i.y * m.m(1)(3) + i.z * m.m(2)(3) + i.w * m.m(3)(3)
    v
  }

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

  def pointAtMatrix(pos: Vec3d, target: Vec3d, up: Vec3d): Mat4x4 = {
    // Calculate new forward direction
    val newForward: Vec3d = subVec(target, pos)
    val normalizednewForward = normalizeVec(newForward)

    // Calculate new up direction
    val a: Vec3d = mulVec(normalizednewForward, dotProduct(up, normalizednewForward))
    val newUp: Vec3d = subVec(up, a)
    val normalizednewUp = normalizeVec(newUp)

    // New right direction
    val newRight: Vec3d = crossProduct(normalizednewUp, normalizednewForward)

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

  def addVec(v1: Vec3d, v2: Vec3d): Vec3d = {
    val vector: Vec3d = new Vec3d
    vector.x = v1.x + v2.x
    vector.y = v1.y + v2.y
    vector.z = v1.z + v2.z
    vector
  }

  def subVec(v1: Vec3d, v2: Vec3d): Vec3d = {
    val vector: Vec3d = new Vec3d
    vector.x = v1.x - v2.x
    vector.y = v1.y - v2.y
    vector.z = v1.z - v2.z
    vector
  }

  def mulVec(v: Vec3d, k: Double): Vec3d = {
    val vector: Vec3d = new Vec3d
    vector.x = v.x * k
    vector.y = v.y * k
    vector.z = v.z * k
    vector
  }

  def divVec(v: Vec3d, k: Double): Vec3d = {
    val vector: Vec3d = new Vec3d
    vector.x = v.x / k
    vector.y = v.y / k
    vector.z = v.z / k
    vector
  }

  def dotProduct(v1: Vec3d, v2: Vec3d): Double = {
    v1.x * v2.x + v1.y * v2.y + v1.z * v2.z
  }

  def length(v: Vec3d): Double = {
    math.sqrt(dotProduct(v, v))
  }

  def normalizeVec(v: Vec3d): Vec3d = {
    val l: Double = length(v)
    v.x /= l; v.y /= l; v.z /= l
    v
  }
  
  def crossProduct(v1: Vec3d, v2: Vec3d): Vec3d = {
    var v: Vec3d = new Vec3d
    v.x = v1.y * v2.z - v1.z * v2.y
    v.y = v1.z * v2.x - v1.x * v2.z
    v.z = v1.x * v2.y - v1.y * v2.x
    v
  }

  def intersectPlane(planeP: Vec3d, planeN: Vec3d, lineStart: Vec3d, lineEnd: Vec3d): Vec3d = {
    val planeNorm: Vec3d = normalizeVec(planeN)
    val planeD: Double = -dotProduct(planeNorm, planeP)
    val ad: Double = dotProduct(lineStart, planeNorm)
    val bd: Double = dotProduct(lineEnd, planeNorm)
    val t: Double = (-planeD - ad) / (bd - ad)
    val lineStartToEnd: Vec3d = subVec(lineEnd, lineStart)
    val lineToIntersect: Vec3d = mulVec(lineStartToEnd, t)
    val ret: Vec3d = addVec(lineStart, lineToIntersect)
    ret
  }

  def clipAgainstPlane(plane_p: Vec3d, plane_n: Vec3d, in_tri: Triangle, out_tri1: Triangle, out_tri2: Triangle): (Int, Triangle, Triangle) = {
		// Make sure plane normal is indeed normal
		val normalizedplane_n: Vec3d = normalizeVec(plane_n)
    var mutableout_tri1: Triangle = out_tri1
    var mutableout_tri2: Triangle = out_tri2

		// Return signed shortest distance from point to plane, plane normal must be normalised
		def dist(p: Vec3d): Double = {
			normalizedplane_n.x * p.x + normalizedplane_n.y * p.y + normalizedplane_n.z * p.z - dotProduct(normalizedplane_n, plane_p)
		}

		// Create two temporary storage arrays to classify points either side of plane
		// If distance sign is positive, point lies on "inside" of plane
		var inside_points: Array[Vec3d] = Array.fill(3)(new Vec3d);  var nInsidePointCount: Int = 0
		var outside_points: Array[Vec3d] = Array.fill(3)(new Vec3d); var nOutsidePointCount: Int = 0

		// Get signed distance of each point in triangle to plane
		val d0: Double = dist(in_tri.p(0))
		val d1: Double = dist(in_tri.p(1))
		val d2: Double = dist(in_tri.p(2))

		if (d0 >= 0) {
      inside_points(nInsidePointCount) = in_tri.p(0); 
      nInsidePointCount += 1
    } else {
      outside_points(nOutsidePointCount) = in_tri.p(0) 
      nOutsidePointCount += 1
    }
		if (d1 >= 0) {
      inside_points(nInsidePointCount) = in_tri.p(1) 
      nInsidePointCount += 1
    } else {
      outside_points(nOutsidePointCount) = in_tri.p(1)
      nOutsidePointCount += 1
    }
		if (d2 >= 0) {
      inside_points(nInsidePointCount) = in_tri.p(2)
      nInsidePointCount += 1
    } else {
      outside_points(nOutsidePointCount) = in_tri.p(2)
      nOutsidePointCount += 1
    }

		// Now classify triangle points, and break the input triangle into 
		// smaller output triangles if required. There are four possible
		// outcomes...

		if (nInsidePointCount == 0)
		{
			// All points lie on the outside of plane, so clip whole triangle
			// It ceases to exist

			return (0, mutableout_tri1, mutableout_tri2) // No returned triangles are valid
		}

		if (nInsidePointCount == 3)
		{
			// All points lie on the inside of plane, so do nothing
			// and allow the triangle to simply pass through
			mutableout_tri1 = in_tri

			return (1, mutableout_tri1, mutableout_tri2) // Just the one returned original triangle is valid
		}

		if (nInsidePointCount == 1 && nOutsidePointCount == 2)
		{
			// Triangle should be clipped. As two points lie outside
			// the plane, the triangle simply becomes a smaller triangle

			// Copy appearance info to new triangle
      mutableout_tri1.brightness = in_tri.brightness
			mutableout_tri1.col =  Color.BLUE//in_tri.col;

			// The inside point is valid, so keep that...
			mutableout_tri1.p(0) = inside_points(0)

			// but the two new points are at the locations where the 
			// original sides of the triangle (lines) intersect with the plane
			mutableout_tri1.p(1) = intersectPlane(plane_p, normalizedplane_n, inside_points(0), outside_points(0))
			mutableout_tri1.p(2) = intersectPlane(plane_p, normalizedplane_n, inside_points(0), outside_points(1))

			return (1, mutableout_tri1, mutableout_tri2) // Return the newly formed single triangle
		}

		if (nInsidePointCount == 2 && nOutsidePointCount == 1)
		{
			// Triangle should be clipped. As two points lie inside the plane,
			// the clipped triangle becomes a "quad". Fortunately, we can
			// represent a quad with two new triangles

			// Copy appearance info to new triangles
      mutableout_tri1.brightness = in_tri.brightness
			mutableout_tri1.col =  Color.GREEN//in_tri.col

      mutableout_tri2.brightness = in_tri.brightness
			mutableout_tri2.col =  Color.RED//in_tri.col

			// The first triangle consists of the two inside points and a new
			// point determined by the location where one side of the triangle
			// intersects with the plane
			mutableout_tri1.p(0) = inside_points(0)
			mutableout_tri1.p(1) = inside_points(1)
			mutableout_tri1.p(2) = intersectPlane(plane_p, normalizedplane_n, inside_points(0), outside_points(0))

			// The second triangle is composed of one of he inside points, a
			// new point determined by the intersection of the other side of the 
			// triangle and the plane, and the newly created point above
			mutableout_tri2.p(0) = inside_points(1)
			mutableout_tri2.p(1) = mutableout_tri1.p(2)
			mutableout_tri2.p(2) = intersectPlane(plane_p, normalizedplane_n, inside_points(1), outside_points(0))

			return (2, mutableout_tri1, mutableout_tri2) // Return two newly formed triangles which form a quad
		}
    (1, mutableout_tri1, mutableout_tri2)
	}

  private def drawTriangle(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double, fillColor: Color, lineColor: Color): Unit = {
    gc.setFill(fillColor)
    gc.setStroke(lineColor)
    gc.fillPolygon(Seq((x1, y1), (x2, y2), (x3, y3)))
    gc.strokePolygon(Seq((x1, y1), (x2, y2), (x3, y3)))
  }

  val canvas = new Canvas(1920, 1080)
  val gc: GraphicsContext = canvas.getGraphicsContext2D()
  gc.setFill(Color.BLACK)
  gc.fillRect(0.0, 0.0, canvas.getWidth(), canvas.getHeight())

  // Load Object File
  val meshCube: Mesh = MeshObjects.loadFromObjectFile("src/main/resources/axis.obj")

  // Projection Matrix
  val matProj: Mat4x4 = makeProjection(90.0, canvas.getHeight() / canvas.getWidth(), 0.1, 1000.0)

  private var vCamera: Vec3d = new Vec3d
  private var vLookDir: Vec3d = new Vec3d
  val fNear: Vec3d = new Vec3d
  fNear.x = 0.0; fNear.y = 0.0; fNear.z = 0.1
  val fFar: Vec3d = new Vec3d
  fFar.x = 0.0; fFar.y = 0.0; fFar.z = 1.0
  val vec1: Vec3d = new Vec3d
  vec1.x = 0.0; vec1.y = 0.0; vec1.z = 0.0
  val vec2: Vec3d = new Vec3d
  vec2.x = 0.0; vec2.y = 1.0; vec2.z = 0.0
  val vec3: Vec3d = new Vec3d
  vec3.x = 0.0; vec3.y = canvas.getHeight() - 1; vec3.z = 0.0
  val vec4: Vec3d = new Vec3d
  vec4.x = 0.0; vec4.y = -1.0; vec4.z = 0.0
  val vec5: Vec3d = new Vec3d
  vec5.x = 1.0; vec5.y = 0.0; vec5.z = 0.0
  val vec6: Vec3d = new Vec3d
  vec6.x = canvas.getWidth() - 1; vec6.y = 0.0; vec6.z = 0.0
  val vec7: Vec3d = new Vec3d
  vec7.x = -1.0; vec7.y = 0.0; vec7.z = 0.0

  private var fYaw: Double = 0.0

  private var fTheta: Double = 0
  
    stage = new JFXApp.PrimaryStage {
      title = "GraphicsEngine3D"
      scene = new Scene(1920, 1080) {
        content += canvas
        var lastTime = -1L
		    val timer = AnimationTimer { time =>
				if(lastTime >= 0) {
					val delay = (time - lastTime) / 1e9
          
          if(upHeld) vCamera.y += 8.0 * delay
          if(downHeld) vCamera.y -= 8.0 * delay
          if(leftHeld) vCamera.x += 8.0 * delay
          if(rightHeld) vCamera.x -= 8.0 * delay

          val vForward: Vec3d = mulVec(vLookDir, 8.0 * delay)

          if(wHeld) vCamera = addVec(vCamera, vForward)
          if(sHeld) vCamera = subVec(vCamera, vForward)

          if(aHeld) fYaw -= 2.0 * delay
          if(dHeld) fYaw += 2.0 * delay

          //fTheta += 1 * delay
          val matRotZ: Mat4x4 = makeRotZ(fTheta * 0.5)
          val matRotX: Mat4x4 = makeRotX(fTheta)
          
          val matTrans: Mat4x4 = makeTranslation(0.0, 0.0, 5.0)

          var matWorld: Mat4x4 = makeIdentity
          matWorld = multiplyMatrix(matRotZ, matRotX)
          matWorld = multiplyMatrix(matWorld, matTrans)

          val vUp: Vec3d = new Vec3d
          vUp.x = 0.0; vUp.y = 1.0; vUp.z = 0.0
          var vTarget: Vec3d = new Vec3d
          vTarget.x = 0.0; vTarget.y = 0.0; vTarget.z = 1.0
          val matCameraRot = makeRotY(fYaw)
          vLookDir = multiplyMatrixVector(matCameraRot, vTarget)
          vTarget = addVec(vCamera, vLookDir)

          val matCamera: Mat4x4 = pointAtMatrix(vCamera, vTarget, vUp)

          // Make view matrix from camera
          val matView: Mat4x4 = quickInverse(matCamera)

          // Store triangles for rastering later
          val vecTrianglesToRaster: ArrayBuffer[Triangle] = ArrayBuffer[Triangle]()

          // Draw Triangles
          for(tri <- meshCube.tris) {
            val triProjected: Triangle = new Triangle
            val triTransformed: Triangle = new Triangle
            val triViewed: Triangle = new Triangle

            triTransformed.p(0) = multiplyMatrixVector(matWorld, tri.p(0))
            triTransformed.p(1) = multiplyMatrixVector(matWorld, tri.p(1))
            triTransformed.p(2) = multiplyMatrixVector(matWorld, tri.p(2))

            val line1: Vec3d = subVec(triTransformed.p(1), triTransformed.p(0))
            val line2: Vec3d = subVec(triTransformed.p(2), triTransformed.p(0))
            var normal: Vec3d = crossProduct(line1, line2)

            normal = normalizeVec(normal)

            val vCameraRay: Vec3d = subVec(triTransformed.p(0), vCamera)
            
            if(dotProduct(normal, vCameraRay) < 0.0) {

              var lightDirection: Vec3d = new Vec3d
              lightDirection.x = 0.0; lightDirection.y = 1.0; lightDirection.z = -1.0
              lightDirection = normalizeVec(lightDirection)

              // Shading value based on alignment of light direction and triangle surface normal
              val dp: Double = math.max(0.1, dotProduct(lightDirection, normal))
              triTransformed.brightness = dp
              triTransformed.col = Color.AZURE

              // Convert world space --> view space
              triViewed.p(0) = multiplyMatrixVector(matView, triTransformed.p(0))
              triViewed.p(1) = multiplyMatrixVector(matView, triTransformed.p(1))
              triViewed.p(2) = multiplyMatrixVector(matView, triTransformed.p(2))
              triViewed.brightness = triTransformed.brightness
              triViewed.col = triTransformed.col

              // Clip Viewed Triangle against near plane, this could form two additional
              // additional triangles. 
              var nClippedTriangles: Int = 0
              val clipped: Array[Triangle] = Array.fill(2)(new Triangle)
              val ret: (Int, Triangle, Triangle) = clipAgainstPlane(fNear, fFar, triViewed, clipped(0), clipped(1))
              nClippedTriangles = ret._1
              clipped(0) = ret._2
              clipped(1) = ret._3

              // We may end up with multiple triangles form the clip, so project as
              // required
              for (n <- 0 until nClippedTriangles) {

                // Project triangles from 3D --> 2D
                triProjected.p(0) = multiplyMatrixVector(matProj, clipped(n).p(0))
                triProjected.p(1) = multiplyMatrixVector(matProj, clipped(n).p(1))
                triProjected.p(2) = multiplyMatrixVector(matProj, clipped(n).p(2))
                triProjected.brightness = clipped(n).brightness
                triProjected.col = clipped(n).col

                // Scale into view and normalize
                triProjected.p(0) = divVec(triProjected.p(0), triProjected.p(0).w)
                triProjected.p(1) = divVec(triProjected.p(1), triProjected.p(1).w)
                triProjected.p(2) = divVec(triProjected.p(2), triProjected.p(2).w)

                // X/Y are inverted so put them back
                triProjected.p(0).x *= -1.0
                triProjected.p(1).x *= -1.0
                triProjected.p(2).x *= -1.0
                triProjected.p(0).y *= -1.0
                triProjected.p(1).y *= -1.0
                triProjected.p(2).y *= -1.0

                // Scale Into View
                val vOffsetView: Vec3d = new Vec3d
                vOffsetView.x = 1.0; vOffsetView.y = 1.0; vOffsetView.z = 0.0
                triProjected.p(0) = addVec(triProjected.p(0), vOffsetView)
                triProjected.p(1) = addVec(triProjected.p(1), vOffsetView)
                triProjected.p(2) = addVec(triProjected.p(2), vOffsetView)

                triProjected.p(0).x *= 0.5 * canvas.getWidth(); triProjected.p(0).y *= 0.5 * canvas.getHeight()
                triProjected.p(1).x *= 0.5 * canvas.getWidth(); triProjected.p(1).y *= 0.5 * canvas.getHeight()
                triProjected.p(2).x *= 0.5 * canvas.getWidth(); triProjected.p(2).y *= 0.5 * canvas.getHeight()

                // Store triangles for sorting
                vecTrianglesToRaster.append(triProjected)
            }
          }
        }

          // Fill Screen
          gc.setFill(Color.BLACK)
          gc.fillRect(0.0, 0.0, canvas.getWidth(), canvas.getHeight())
          val arrayTriangles: ArrayBuffer[Triangle] = ArrayBuffer[Triangle]()
          for(triToRaster <- vecTrianglesToRaster) {
            // Clip triangles against all four screen edges, this could yield a bunch of triangles
            val clipped: Array[Triangle] = Array.fill(2)(new Triangle)
            arrayTriangles.append(triToRaster)
            var nNewTriangles: Int = 1
            for(p <- 0 until 4) {
              var nTrisToAdd: Int = 0
              while(nNewTriangles > 0) {
                val test: Triangle = arrayTriangles(0)
                arrayTriangles.remove(0)
                nNewTriangles -= 1

                p match {
                  case 0 =>
                    val ret = clipAgainstPlane(vec1, vec2, test, clipped(0), clipped(1))
                    nTrisToAdd = ret._1
                    clipped(0) = ret._2
                    clipped(1) = ret._3
                  case 1 =>
                    val ret = clipAgainstPlane(vec3, vec4, test, clipped(0), clipped(1))
                    nTrisToAdd = ret._1
                    clipped(0) = ret._2
                    clipped(1) = ret._3
                  case 2 =>
                    val ret = clipAgainstPlane(vec1, vec5, test, clipped(0), clipped(1))
                    nTrisToAdd = ret._1
                    clipped(0) = ret._2
                    clipped(1) = ret._3
                  case 3 =>
                    val ret = clipAgainstPlane(vec6, vec7, test, clipped(0), clipped(1))
                    nTrisToAdd = ret._1
                    clipped(0) = ret._2
                    clipped(1) = ret._3
                }
                for(w <- 0 until nTrisToAdd) arrayTriangles.append(clipped(w))
              }
              nNewTriangles = arrayTriangles.length
            }
            // Rasterize Triangle
            for(t <- arrayTriangles) {
              val color = Color.hsb(t.col.hue, 0.5, t.brightness)
              drawTriangle(t.p(0).x, t.p(0).y, t.p(1).x, t.p(1).y, t.p(2).x, t.p(2).y, color, Color.BLACK)
            }
          }
				}
				lastTime = time
			}
			timer.start()

      onKeyPressed = (ke: KeyEvent) => {
          ke.code match {
            case KeyCode.Up => upHeld = true
            case KeyCode.Down => downHeld = true
            case KeyCode.Left => leftHeld = true
            case KeyCode.Right => rightHeld = true
            case KeyCode.A => aHeld = true
            case KeyCode.W => wHeld = true
            case KeyCode.S => sHeld = true
            case KeyCode.D => dHeld = true
            case _ =>
          }
        }

        onKeyReleased = (ke: KeyEvent) => {
          ke.code match {
            case KeyCode.Up => upHeld = false
            case KeyCode.Down => downHeld = false
            case KeyCode.Left => leftHeld = false
            case KeyCode.Right => rightHeld = false
            case KeyCode.A => aHeld = false
            case KeyCode.W => wHeld = false
            case KeyCode.S => sHeld = false
            case KeyCode.D => dHeld = false
            case _ =>
          }
        }

      }
    }
}