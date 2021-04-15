package graphicsengine3d

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.canvas.Canvas
import scalafx.scene.Scene
import scalafx.animation.AnimationTimer
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
object Main extends JFXApp {

  private var fTheta: Double = 0

  def multiplyMatrixVector(i: Vec3d, o: Vec3d, m: Mat4x4) = {
    o.x = i.x * m.m(0)(0) + i.y * m.m(1)(0) + i.z * m.m(2)(0) + m.m(3)(0)
    o.y = i.x * m.m(0)(1) + i.y * m.m(1)(1) + i.z * m.m(2)(1) + m.m(3)(1)
    o.z = i.x * m.m(0)(2) + i.y * m.m(1)(2) + i.z * m.m(2)(2) + m.m(3)(2)
    val w: Double = i.x * m.m(0)(3) + i.y * m.m(1)(3) + i.z * m.m(2)(3) + m.m(3)(3)

    if(w != 0) {
      o.x /= w
      o.y /= w
      o.z /= w
    }
  }

  def drawLine(x1: Double, y1: Double, x2: Double, y2: Double, gc: GraphicsContext) = {
    gc.beginPath()
    gc.moveTo(x1, y1)
    gc.lineTo(x2, y2)
    gc.setStroke(Color.WHITE)
    gc.stroke()
  }

  def drawTriangle(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double) = {
    drawLine(x1, y1, x2, y2, gc)
    drawLine(x2, y2, x3, y3, gc)
    drawLine(x3, y3, x1, y1, gc)
  }

  val canvas = new Canvas(1920, 1080)
  val gc: GraphicsContext = canvas.getGraphicsContext2D()
  gc.fillRect(0.0, 0.0, canvas.getWidth(), canvas.getHeight())
  val meshCube: Mesh = new Mesh
  val matProj: Mat4x4 = new Mat4x4
  val vec1 = new Vec3d
  meshCube.tris = List(
    	
    // SOUTH
		( vec1.x = 0.0, 0.0, 0.0,    0.0, 1.0, 0.0,    1.0, 1.0, 0.0 ),
		( 0.0, 0.0, 0.0,    1.0, 1.0, 0.0,    1.0, 0.0, 0.0 ),

		// EAST                                                      
		( 1.0, 0.0, 0.0,    1.0, 1.0, 0.0,    1.0, 1.0, 1.0 ),
		( 1.0, 0.0, 0.0,    1.0, 1.0, 1.0,    1.0, 0.0, 1.0 ),

		// NORTH                                                     
		( 1.0, 0.0, 1.0,    1.0, 1.0, 1.0,    0.0, 1.0, 1.0 ),
		( 1.0, 0.0, 1.0,    0.0, 1.0, 1.0,    0.0, 0.0, 1.0 ),

		// WEST                                                      
		( 0.0, 0.0, 1.0,    0.0, 1.0, 1.0,    0.0, 1.0, 0.0 ),
		( 0.0, 0.0, 1.0,    0.0, 1.0, 0.0,    0.0, 0.0, 0.0 ),

		// TOP                                                       
		( 0.0, 1.0, 0.0,    0.0, 1.0, 1.0,    1.0, 1.0, 1.0 ),
		( 0.0, 1.0, 0.0,    1.0, 1.0, 1.0,    1.0, 1.0, 0.0 ),

		// BOTTOM                                                    
		( 1.0, 0.0, 1.0,    0.0, 0.0, 1.0,    0.0, 0.0, 0.0 ),
		( 1.0, 0.0, 1.0,    0.0, 0.0, 0.0,    1.0, 0.0, 0.0 ),

		)
  // Projection Matrix
  val fNear: Double = 0.1
  val fFar: Double = 1000.0
  val fFov: Double = 90.0
  val fAspectRatio: Double = canvas.getHeight / canvas.getWidth
  val fFovRad: Double = 1.0 / math.tan(fFov * 0.5 / 180.0 * math.Pi)
  matProj.m(0)(0) = fAspectRatio * fFovRad
  matProj.m(1)(1) = fFovRad
  matProj.m(2)(2) = fFar / (fFar - fNear)
  matProj.m(3)(2) = (-fFar * fNear) / (fFar - fNear)
  matProj.m(2)(3) = 1.0
  matProj.m(3)(3) = 0.0
    stage = new JFXApp.PrimaryStage {
      title = "GraphicsEngine3D"
      scene = new Scene(1920, 1080) {
        content += canvas
        var lastTime = -1L
		    val timer = AnimationTimer { time =>
				if(lastTime >= 0) {
					val delay = (time - lastTime) / 1e9

          val matRotZ: Mat4x4 = new Mat4x4
          val matRotX: Mat4x4= new Mat4x4
          fTheta += 1 * delay

          // Rotation Z
          matRotZ.m(0)(0) = math.cos(fTheta);
          matRotZ.m(0)(1) = math.sin(fTheta);
          matRotZ.m(1)(0) = -math.sin(fTheta);
          matRotZ.m(1)(1) = math.cos(fTheta);
          matRotZ.m(2)(2) = 1;
          matRotZ.m(3)(3) = 1;

          // Rotation X
          matRotX.m(0)(0) = 1;
          matRotX.m(1)(1) = math.cos(fTheta * 0.5);
          matRotX.m(1)(2) = math.sin(fTheta * 0.5);
          matRotX.m(2)(1) = -math.sin(fTheta * 0.5);
          matRotX.m(2)(2) = math.cos(fTheta * 0.5);
          matRotX.m(3)(3) = 1;

          for(tri <- meshCube.tris) {
            val triProjected: Triangle = new Triangle
            val triRotatedZ: Triangle = new Triangle
            val triRotatedZX: Triangle = new Triangle

            // Rotate in Z-Axis
            multiplyMatrixVector(tri.p(0), triRotatedZ.p(0), matRotZ);
            multiplyMatrixVector(tri.p(1), triRotatedZ.p(1), matRotZ);
            multiplyMatrixVector(tri.p(2), triRotatedZ.p(2), matRotZ);

            // Rotate in X-Axis
            multiplyMatrixVector(triRotatedZ.p(0), triRotatedZX.p(0), matRotX);
            multiplyMatrixVector(triRotatedZ.p(1), triRotatedZX.p(1), matRotX);
            multiplyMatrixVector(triRotatedZ.p(2), triRotatedZX.p(2), matRotX);

            val triTranslated: Triangle = triRotatedZX

            triTranslated.p(0).z = triRotatedZX.p(0).z + 3
            triTranslated.p(1).z = triRotatedZX.p(1).z + 3
            triTranslated.p(2).z = triRotatedZX.p(2).z + 3

            multiplyMatrixVector(triTranslated.p(0), triProjected.p(0), matProj)
            multiplyMatrixVector(triTranslated.p(1), triProjected.p(1), matProj)
            multiplyMatrixVector(triTranslated.p(2), triProjected.p(2), matProj)

            // Scale Into View
            triProjected.p(0).x += 1; triProjected.p(0).y += 1
            triProjected.p(1).x += 1; triProjected.p(1).y += 1
            triProjected.p(2).x += 1; triProjected.p(2).y += 1

            triProjected.p(0).x *= 0.5 * canvas.getWidth(); triProjected.p(0).y *= 0.5 * canvas.getHeight()
            triProjected.p(1).x *= 0.5 * canvas.getWidth(); triProjected.p(1).y *= 0.5 * canvas.getHeight()
            triProjected.p(2).x *= 0.5 * canvas.getWidth(); triProjected.p(2).y *= 0.5 * canvas.getHeight()

            drawTriangle(triProjected.p(0).x, triProjected.p(0).y, triProjected.p(1).x, triProjected.p(1).y, triProjected.p(2).x, triProjected.p(2).x)
          }
				}
				lastTime = time
			}
			timer.start()
      }
    }
}