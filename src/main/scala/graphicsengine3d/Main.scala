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
import scalafx.scene.image.Image

object Main extends JFXApp {

  private var upHeld = false
  private var downHeld = false
  private var leftHeld = false
  private var rightHeld = false
  private var aHeld = false
  private var wHeld = false
  private var sHeld = false
  private var dHeld = false

  val canvas = new Canvas(1920, 1080)
  val gc: GraphicsContext = canvas.getGraphicsContext2D()
  gc.setFill(Color.Black)
  gc.fillRect(0.0, 0.0, canvas.getWidth(), canvas.getHeight())
  // Load Object File
  val meshCube: Mesh = MeshObjects.loadObjectFromFile("src/main/resources/VideoShip.obj")

  // Projection Matrix
  val matrix: Mat4x4 = new Mat4x4
  val matProj: Mat4x4 = matrix.makeProjection(90.0, canvas.getHeight() / canvas.getWidth(), 0.1, 1000.0)
  val vCamera: Vec3d = new Vec3d
  val vLookDir: Vec3d = new Vec3d
  private var fYaw: Double = 0.0
  private var fTheta: Double = 0
  val sprTex1: Image = new Image("file:src/main/resources/floor.png")
  
  stage = new JFXApp.PrimaryStage {
    title = "GraphicsEngine3D"
    scene = new Scene(1920, 1080) {
      content += canvas
      var lastTime = -1L
      var nextFrame: Boolean = false
      val timer = AnimationTimer { time =>
        if(lastTime >= 0) {
          val delay = (time - lastTime) / 1e9
          
          val vForward: Vec3d = vLookDir * (8.0 * delay)
          
          if(upHeld) vCamera.y += 8.0 * delay
          if(downHeld) vCamera.y -= 8.0 * delay
          if(leftHeld) vCamera.x -= 8.0 * delay
          if(rightHeld) vCamera.x += 8.0 * delay


          if(wHeld) vCamera += vForward
          if(sHeld) vCamera -= vForward

          if(aHeld) fYaw -= 2.0 * delay
          if(dHeld) fYaw += 2.0 * delay

          //fTheta += 1 * delay
          val matRotZ: Mat4x4 = matrix.makeRotZ(fTheta * 0.5)
          val matRotX: Mat4x4 = matrix.makeRotX(fTheta)
          
          val matTrans: Mat4x4 = matrix.makeTranslation(0.0, 0.0, 5.0)

          val matWorld: Mat4x4 = matrix.makeIdentity.multiplyMatrix(matRotZ, matRotX).multiplyByMatrix(matTrans)

          val vUp: Vec3d = new Vec3d(0.0, 1.0, 0.0)
          val ovTarget: Vec3d = new Vec3d(0.0, 0.0, 1.0)
          val matCameraRot = matrix.makeRotY(fYaw)
          vLookDir.multiplyMatrixVector(matCameraRot, ovTarget)
          val vTarget: Vec3d = vCamera + vLookDir

          val matCamera: Mat4x4 = matrix.pointAtMatrix(vCamera, vTarget, vUp)

          // Make view matrix from camera
          val matView: Mat4x4 = matCamera.quickInverse(matCamera)

          // Store triangles for rastering later
          val vecTrianglesToRasterUnsorted: ArrayBuffer[Triangle] = ArrayBuffer[Triangle]()

          // Draw Triangles
          for(tri <- meshCube.tris) {
            
            val triTransformed: Triangle = new Triangle
            triTransformed.p(0).multiplyMatrixVector(matWorld, tri.p(0))
            triTransformed.p(1).multiplyMatrixVector(matWorld, tri.p(1))
            triTransformed.p(2).multiplyMatrixVector(matWorld, tri.p(2))
            triTransformed.t(0) = tri.t(0)
            triTransformed.t(1) = tri.t(1)
            triTransformed.t(2) = tri.t(2)

            val line1: Vec3d = triTransformed.p(1) - triTransformed.p(0)
            val line2: Vec3d = triTransformed.p(2) - triTransformed.p(0)
            val normal: Vec3d = line1.crossProduct(line2).normalize

            val vCameraRay: Vec3d = triTransformed.p(0) - vCamera
            
            if(normal.dotProduct(vCameraRay) < 0.0) {

              val lightDirection: Vec3d = new Vec3d(0.0, 1.0, -1.0).normalize

              // Shading value based on alignment of light direction and triangle surface normal
              val dp: Double = math.max(0.1, lightDirection.dotProduct(normal))
              
              val bri: Double = dp
              val sat: Double = 0.0
              val col: Color = Color.DarkGreen

              triTransformed.bri = bri
              triTransformed.sat = sat
              triTransformed.col = col

              // Convert world space --> view space
              val triViewed: Triangle = new Triangle
              triViewed.p(0).multiplyMatrixVector(matView, triTransformed.p(0))
              triViewed.p(1).multiplyMatrixVector(matView, triTransformed.p(1))
              triViewed.p(2).multiplyMatrixVector(matView, triTransformed.p(2))
              triViewed.t(0) = triTransformed.t(0)
              triViewed.t(1) = triTransformed.t(1)
              triViewed.t(2) = triTransformed.t(2)
              triViewed.bri = triTransformed.bri
              triViewed.sat = triTransformed.sat
              triViewed.col = triTransformed.col

              // Clip Viewed Triangle against near plane, this could form two additional
              // additional triangles.
              var nClippedTriangles: Int = 0
              val clipped: Array[Triangle] = Array.fill(2)(new Triangle)
              val ret = triViewed.clipAgainstPlane(new Vec3d(0.0, 0.0, 0.1), new Vec3d(0.0, 0.0, 1.0))
              nClippedTriangles = ret._1
              clipped(0) = ret._2
              clipped(1) = ret._3

              // We may end up with multiple triangles form the clip, so project as
              // required
              for (n <- 0 until nClippedTriangles) {

                // Project triangles from 3D --> 2D
                val triProjected: Triangle = new Triangle
                triProjected.p(0).multiplyMatrixVector(matProj, clipped(n).p(0))
                triProjected.p(1).multiplyMatrixVector(matProj, clipped(n).p(1))
                triProjected.p(2).multiplyMatrixVector(matProj, clipped(n).p(2))
                triProjected.t(0) = clipped(n).t(0)
                triProjected.t(1) = clipped(n).t(1)
                triProjected.t(2) = clipped(n).t(2)
                triProjected.bri = clipped(n).bri
                triProjected.sat = clipped(n).sat
                triProjected.col = clipped(n).col

                // Scale into view and normalize
                triProjected.p(0) = triProjected.p(0) / triProjected.p(0).w
                triProjected.p(1) = triProjected.p(1) / triProjected.p(1).w
                triProjected.p(2) = triProjected.p(2) / triProjected.p(2).w

                // X/Y are inverted so put them back
                triProjected.p(0).x *= -1.0; triProjected.p(0).y *= -1.0
                triProjected.p(1).x *= -1.0; triProjected.p(1).y *= -1.0
                triProjected.p(2).x *= -1.0; triProjected.p(2).y *= -1.0

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
                clipped(n) = new Triangle
              }
            }
          }

          val vecTrianglesToRaster: ArrayBuffer[Triangle] = vecTrianglesToRasterUnsorted.sortWith(_.triSort(_))

          // Fill Screen
          gc.setFill(Color.Black)
          gc.fillRect(0.0, 0.0, canvas.getWidth(), canvas.getHeight())

          for(triToRaster <- vecTrianglesToRaster) {
            // Clip triangles against all four screen edges, this could yield a bunch of triangles
            val clipped: Array[Triangle] = Array.fill(2)(new Triangle)
            val arrayTriangles: ArrayBuffer[Triangle] = ArrayBuffer[Triangle]()
            
            arrayTriangles += triToRaster
            var nNewTriangles: Int = 1

            for(p <- 0 until 4) {
              var nTrisToAdd: Int = 0
              while(nNewTriangles > 0) {
                val test: Triangle = arrayTriangles.remove(0)
                nNewTriangles -= 1
                nTrisToAdd = p match {
                  case 0 =>
                    val ret = test.clipAgainstPlane(new Vec3d(0.0, 0.0, 0.0), new Vec3d(0.0, 1.0, 0.0))
                    clipped(0) = ret._2
                    clipped(1) = ret._3
                    ret._1
                  case 1 =>
                    val ret = test.clipAgainstPlane(new Vec3d(0.0, canvas.getHeight() - 1.0, 0.0), new Vec3d(0.0, -1.0, 0.0))
                    clipped(0) = ret._2
                    clipped(1) = ret._3
                    ret._1
                  case 2 =>
                    val ret = test.clipAgainstPlane(new Vec3d(0.0, 0.0, 0.0), new Vec3d(1.0, 0.0, 0.0))
                    clipped(0) = ret._2
                    clipped(1) = ret._3
                    ret._1
                  case 3 =>
                    val ret = test.clipAgainstPlane(new Vec3d(canvas.getWidth() - 1.0, 0.0, 0.0), new Vec3d(-1.0, 0.0, 0.0))
                    clipped(0) = ret._2
                    clipped(1) = ret._3
                    ret._1
                  case _ => 0
                }
                for(w <- 0 until nTrisToAdd) {
                  arrayTriangles += clipped(w)
                }
              }
              nNewTriangles = arrayTriangles.length
            }
            
            // Rasterize Triangle
            for(t <- arrayTriangles) {
              val color: Color = Color.hsb(t.col.hue, t.sat, t.bri)
              //t.texturedTriangle(gc, sprTex1)
              t.fill(sprTex1, gc, Color.Transparent, Color.White)
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
            case KeyCode.N => nextFrame = true
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