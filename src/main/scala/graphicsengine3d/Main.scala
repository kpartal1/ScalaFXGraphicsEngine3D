package graphicsengine3d

import scalafx.application.JFXApp
import scalafx.scene.canvas.Canvas
import scalafx.scene.Scene

object Main extends JFXApp {
	val canvas = new Canvas(1920, 1080)
	stage = new JFXApp.PrimaryStage {
		title = "GraphicsEngine3D"
		scene = new Scene(1920, 1080) {
		}
	}
}