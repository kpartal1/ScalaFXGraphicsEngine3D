package graphicsengine3d

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.canvas.Canvas
import scalafx.scene.Scene
import scalafx.animation.AnimationTimer
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scala.io.Source
import scalafx.scene.input.KeyEvent
import scalafx.scene.input.KeyCode
import scala.collection.mutable.ArrayBuffer

object Main extends JFXApp {

  private var upHeld = false
  private var downHeld = false
  private var leftHeld = false
  private var rightHeld = false
  private var aHeld = false
  private var wHeld = false
  private var sHeld = false
  private var dHeld = false

  def intersectPlane(planeP: Vec3d, planeN: Vec3d, lineStart: Vec3d, lineEnd: Vec3d): Vec3d = {
    val planeNorm: Vec3d = planeN.normalize
    val planeD: Double = -planeNorm.dotProduct(planeP)
    val ad: Double = lineStart.dotProduct(planeNorm)
    val bd: Double = lineEnd.dotProduct(planeNorm)
    val t: Double = (-planeD - ad) / (bd - ad)
    val lineStartToEnd: Vec3d = lineEnd - lineStart
    val lineToIntersect: Vec3d = lineStartToEnd * t
    val ret: Vec3d = lineStart + lineToIntersect
    ret
  }

  def clipAgainstPlane(plane_p: Vec3d, plane_n: Vec3d, in_tri: Triangle, out_tri1: Triangle, out_tri2: Triangle): Int = {
		// Make sure plane normal is indeed normal
		val normalizedplane_n: Vec3d = plane_n.normalize

		// Return signed shortest distance from point to plane, plane normal must be normalised
		def dist(p: Vec3d) = {
			normalizedplane_n.x * p.x + normalizedplane_n.y * p.y + normalizedplane_n.z * p.z - normalizedplane_n.dotProduct(plane_p)
		}

		// Create two temporary storage arrays to classify points either side of plane
		// If distance sign is positive, point lies on "inside" of plane
		val inside_points: Array[Vec3d] = Array.fill(3)(new Vec3d);  var nInsidePointCount: Int = 0
		val outside_points: Array[Vec3d] = Array.fill(3)(new Vec3d); var nOutsidePointCount: Int = 0

		// Get signed distance of each point in triangle to plane
		val d0: Double = dist(in_tri.p(0))
		val d1: Double = dist(in_tri.p(1))
		val d2: Double = dist(in_tri.p(2))

		if (d0 >= 0) {
      inside_points(nInsidePointCount) = in_tri.p(0)
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

			return 0 // No returned triangles are valid
		}

		if (nInsidePointCount == 3)
		{
			// All points lie on the inside of plane, so do nothing
			// and allow the triangle to simply pass through
			out_tri1.p(0) = in_tri.p(0)
      out_tri1.p(1) = in_tri.p(1)
      out_tri1.p(2) = in_tri.p(2)
      out_tri1.brightness = in_tri.brightness
			out_tri1.col =  in_tri.col

			return 1 // Just the one returned original triangle is valid
		}

		if (nInsidePointCount == 1 && nOutsidePointCount == 2)
		{
			// Triangle should be clipped. As two points lie outside
			// the plane, the triangle simply becomes a smaller triangle

			// Copy appearance info to new triangle
      out_tri1.brightness = in_tri.brightness
			out_tri1.col =  Color.BLUE//in_tri.col;

			// The inside point is valid, so keep that...
			out_tri1.p(0) = inside_points(0)

			// but the two new points are at the locations where the 
			// original sides of the triangle (lines) intersect with the plane
			out_tri1.p(1) = intersectPlane(plane_p, normalizedplane_n, inside_points(0), outside_points(0))
			out_tri1.p(2) = intersectPlane(plane_p, normalizedplane_n, inside_points(0), outside_points(1))

			return 1 // Return the newly formed single triangle
		}

		if (nInsidePointCount == 2 && nOutsidePointCount == 1)
		{
			// Triangle should be clipped. As two points lie inside the plane,
			// the clipped triangle becomes a "quad". Fortunately, we can
			// represent a quad with two new triangles

			// Copy appearance info to new triangles
      out_tri1.brightness = in_tri.brightness
			out_tri1.col =  Color.GREEN//in_tri.col

      out_tri2.brightness = in_tri.brightness
			out_tri2.col =  Color.RED//in_tri.col

			// The first triangle consists of the two inside points and a new
			// point determined by the location where one side of the triangle
			// intersects with the plane
			out_tri1.p(0) = inside_points(0)
			out_tri1.p(1) = inside_points(1)
			out_tri1.p(2) = intersectPlane(plane_p, normalizedplane_n, inside_points(0), outside_points(0))

			// The second triangle is composed of one of he inside points, a
			// new point determined by the intersection of the other side of the 
			// triangle and the plane, and the newly created point above
			out_tri2.p(0) = inside_points(1)
			out_tri2.p(1) = out_tri1.p(2)
			out_tri2.p(2) = intersectPlane(plane_p, normalizedplane_n, inside_points(1), outside_points(0))

			return 2 // Return two newly formed triangles which form a quad
		}
    1
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
  val meshCube: Mesh = MeshObjects.loadFromObjectFile("src/main/resources/Axis.obj")

  // Projection Matrix
  val matrix: Mat4x4 = new Mat4x4
  val matProj: Mat4x4 = matrix.makeProjection(90.0, canvas.getHeight() / canvas.getWidth(), 0.1, 1000.0)

  private var vCamera: Vec3d = new Vec3d
  val vLookDir: Vec3d = new Vec3d

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
          if(leftHeld) vCamera.x -= 8.0 * delay
          if(rightHeld) vCamera.x += 8.0 * delay

          val vForward: Vec3d = vLookDir * (8.0 * delay)

          if(wHeld) vCamera = vCamera + vForward
          if(sHeld) vCamera = vCamera - vForward

          if(aHeld) fYaw -= 2.0 * delay
          if(dHeld) fYaw += 2.0 * delay

          //fTheta += 1 * delay
          val matRotZ: Mat4x4 = matrix.makeRotZ(fTheta * 0.5)
          val matRotX: Mat4x4 = matrix.makeRotX(fTheta)
          
          val matTrans: Mat4x4 = matrix.makeTranslation(0.0, 0.0, 5.0)

          val matWorld: Mat4x4 = matrix.makeIdentity.multiplyMatrix(matRotZ, matRotX).multiplyByMatrix(matTrans)

          val vUp: Vec3d = new Vec3d(0.0, 1.0, 0.0)
          var vTarget: Vec3d = new Vec3d(0.0, 0.0, 1.0)
          val matCameraRot = matrix.makeRotY(fYaw)
          vLookDir.multiplyMatrixVector(matCameraRot, vTarget)
          vTarget = vCamera + vLookDir

          val matCamera: Mat4x4 = matrix.pointAtMatrix(vCamera, vTarget, vUp)

          // Make view matrix from camera
          val matView: Mat4x4 = matCamera.quickInverse(matCamera)

          // Store triangles for rastering later
          val vecTrianglesToRasterUnsorted: ArrayBuffer[Triangle] = ArrayBuffer[Triangle]()

          // Draw Triangles
          for(tri <- meshCube.tris) {
            val triProjected: Triangle = new Triangle
            val triTransformed: Triangle = new Triangle
            val triViewed: Triangle = new Triangle

            triTransformed.p(0).multiplyMatrixVector(matWorld, tri.p(0))
            triTransformed.p(1).multiplyMatrixVector(matWorld, tri.p(1))
            triTransformed.p(2).multiplyMatrixVector(matWorld, tri.p(2))

            val line1: Vec3d = triTransformed.p(1) - triTransformed.p(0)
            val line2: Vec3d = triTransformed.p(2) - triTransformed.p(0)
            val normal: Vec3d = line1.crossProduct(line2).normalize

            val vCameraRay: Vec3d = triTransformed.p(0) - vCamera
            
            if(normal.dotProduct(vCameraRay) < 0.0) {

              val lightDirection: Vec3d = new Vec3d(0.0, 1.0, -1.0).normalize

              // Shading value based on alignment of light direction and triangle surface normal
              val dp: Double = math.max(0.1, lightDirection.dotProduct(normal))
              triTransformed.brightness = dp
              triTransformed.col = Color.ORANGE

              // Convert world space --> view space
              triViewed.p(0).multiplyMatrixVector(matView, triTransformed.p(0))
              triViewed.p(1).multiplyMatrixVector(matView, triTransformed.p(1))
              triViewed.p(2).multiplyMatrixVector(matView, triTransformed.p(2))
              triViewed.brightness = triTransformed.brightness
              triViewed.col = triTransformed.col

              // Clip Viewed Triangle against near plane, this could form two additional
              // additional triangles. 
              var nClippedTriangles: Int = 0
              val clipped: Array[Triangle] = Array.fill(2)(new Triangle)
              nClippedTriangles = clipAgainstPlane(new Vec3d(0.0, 0.0, 0.1), new Vec3d(0.0, 0.0, 1.0), triViewed, clipped(0), clipped(1))

              // We may end up with multiple triangles form the clip, so project as
              // required
              for (n <- 0 until nClippedTriangles) {

                // Project triangles from 3D --> 2D
                triProjected.p(0).multiplyMatrixVector(matProj, clipped(n).p(0))
                triProjected.p(1).multiplyMatrixVector(matProj, clipped(n).p(1))
                triProjected.p(2).multiplyMatrixVector(matProj, clipped(n).p(2))
                triProjected.brightness = clipped(n).brightness
                triProjected.col = clipped(n).col

                // Scale into view and normalize
                triProjected.p(0) = triProjected.p(0) / triProjected.p(0).w
                triProjected.p(1) = triProjected.p(1) / triProjected.p(1).w
                triProjected.p(2) = triProjected.p(2) / triProjected.p(2).w

                // X/Y are inverted so put them back
                triProjected.p(0).x *= -1.0
                triProjected.p(1).x *= -1.0
                triProjected.p(2).x *= -1.0
                triProjected.p(0).y *= -1.0
                triProjected.p(1).y *= -1.0
                triProjected.p(2).y *= -1.0

                // Scale Into View
                val vOffsetView: Vec3d = new Vec3d(1.0, 1.0, 0.0)
                triProjected.p(0) = triProjected.p(0) + vOffsetView
                triProjected.p(1) = triProjected.p(1) + vOffsetView
                triProjected.p(2) = triProjected.p(2) + vOffsetView

                triProjected.p(0).x *= 0.5 * canvas.getWidth(); triProjected.p(0).y *= 0.5 * canvas.getHeight()
                triProjected.p(1).x *= 0.5 * canvas.getWidth(); triProjected.p(1).y *= 0.5 * canvas.getHeight()
                triProjected.p(2).x *= 0.5 * canvas.getWidth(); triProjected.p(2).y *= 0.5 * canvas.getHeight()

                // Store triangles for sorting
                vecTrianglesToRasterUnsorted += triProjected
            }
          }
        }

        val vecTrianglesToRaster: ArrayBuffer[Triangle] = vecTrianglesToRasterUnsorted.sortWith(_.triSort(_))

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
              val test: Triangle = arrayTriangles.remove(0)
              nNewTriangles -= 1

              p match {
                case 0 =>
                  nTrisToAdd = clipAgainstPlane(new Vec3d(0.0, 0.0, 0.0), new Vec3d(0.0, 1.0, 0.0), test, clipped(0), clipped(1))
                case 1 =>
                  nTrisToAdd = clipAgainstPlane(new Vec3d(0.0, canvas.getHeight() - 1.0, 0.0), new Vec3d(0.0, -1.0, 0.0), test, clipped(0), clipped(1))
                case 2 =>
                  nTrisToAdd = clipAgainstPlane(new Vec3d(0.0, 0.0, 0.0), new Vec3d(1.0, 0.0, 0.0), test, clipped(0), clipped(1))
                case 3 =>
                  nTrisToAdd = clipAgainstPlane(new Vec3d(canvas.getWidth() - 1.0, 0.0, 0.0), new Vec3d(-1.0, 0.0, 0.0), test, clipped(0), clipped(1))
              }
              for(w <- 0 until nTrisToAdd) arrayTriangles += clipped(w)
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