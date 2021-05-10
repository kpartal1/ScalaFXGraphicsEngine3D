package graphicsengine3d

import scalafx.scene.paint.Color
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.image.Image

class Triangle {
  var p: Array[Vec3d] = Array.fill(3)(new Vec3d)
	var t: Array[Vec2d] = Array.fill(3)(new Vec2d)
  var bri: Double = 1.0
  var col: Color = Color.Black
	var sat: Double = 1.0
	
  def apply(vec1: Vec3d, vec2: Vec3d, vec3: Vec3d): Unit = {
    this.p(0) = vec1; this.p(1) = vec2; this.p(2) = vec3
  }

	def applyTex(vec1: Vec2d, vec2: Vec2d, vec3: Vec2d): Unit = {
		this.t(0) = vec1; this.t(1) = vec2; this.t(2) = vec3
	}
  
  def triSort(t: Triangle): Boolean = {
    val z1: Double = (this.p(0).z + this.p(1).z + this.p(2).z) / 3.0
    val z2: Double = (t.p(0).z + t.p(1).z + t.p(2).z) / 3.0
    z1 > z2
  }

  def fill(gc: GraphicsContext, fillColor: Color, strokeColor: Color): Unit = {
    gc.setFill(fillColor)
    gc.fillPolygon(Seq((this.p(0).x, this.p(0).y), (this.p(1).x, this.p(1).y), (this.p(2).x, this.p(2).y)))
		gc.setStroke(strokeColor)
    gc.strokePolygon(Seq((this.p(0).x, this.p(0).y), (this.p(1).x, this.p(1).y), (this.p(2).x, this.p(2).y)))
  }
  
  def intersectPlane(planeP: Vec3d, plane_n: Vec3d, lineStart: Vec3d, lineEnd: Vec3d): (Vec3d, Double) = {
    val planeN: Vec3d = plane_n.normalize
    val planeD: Double = -planeN.dotProduct(planeP)
    val ad: Double = lineStart.dotProduct(planeN)
    val bd: Double = lineEnd.dotProduct(planeN)
    val t: Double = (-planeD - ad) / (bd - ad)
    val lineStartToEnd: Vec3d = lineEnd - lineStart
    val lineToIntersect: Vec3d = lineStartToEnd * t
    return (lineStart + lineToIntersect, t)
  }

	override def toString: String = {
		"(" + this.p(0).x + ", " + this.p(0).y + ", " + this.p(0).z + ")\n" +
		"(" + this.p(1).x + ", " + this.p(1).y + ", " + this.p(1).z + ")\n" +
		"(" + this.p(2).x + ", " + this.p(2).y + ", " + this.p(2).z + ")"
	}

  def clipAgainstPlane(plane_p: Vec3d, planeN: Vec3d): (Int, Triangle, Triangle) = {
		// Make sure plane normal is indeed normal
		val plane_n: Vec3d = planeN.normalize
		val out_tri1: Triangle = new Triangle
		val out_tri2: Triangle = new Triangle

		// Return signed shortest distance from point to plane, plane normal must be normalised
		def dist(p: Vec3d) = {
			plane_n.x * p.x + plane_n.y * p.y + plane_n.z * p.z - plane_n.dotProduct(plane_p)
		}

		// Create two temporary storage arrays to classify points either side of plane
		// If distance sign is positive, point lies on "inside" of plane
		val inside_points: Array[Vec3d] = Array.fill(3)(new Vec3d);  var nInsidePointCount: Int = 0
		val outside_points: Array[Vec3d] = Array.fill(3)(new Vec3d); var nOutsidePointCount: Int = 0
		val inside_tex: Array[Vec2d] = Array.fill(3)(new Vec2d); var nInsideTexCount: Int = 0
		val outside_tex: Array[Vec2d] = Array.fill(3)(new Vec2d); var nOutsideTexCount: Int = 0

		// Get signed distance of each point in triangle to plane
		val d0: Double = dist(this.p(0))
		val d1: Double = dist(this.p(1))
		val d2: Double = dist(this.p(2))

		if (d0 >= 0) {
      inside_points(nInsidePointCount) = this.p(0)
      nInsidePointCount += 1
			inside_tex(nInsideTexCount) = this.t(0)
			nInsideTexCount += 1
    } else {
      outside_points(nOutsidePointCount) = this.p(0) 
      nOutsidePointCount += 1
			outside_tex(nOutsideTexCount) = this.t(0)
			nOutsideTexCount += 1
    }
		if (d1 >= 0) {
      inside_points(nInsidePointCount) = this.p(1) 
      nInsidePointCount += 1
			inside_tex(nInsideTexCount) = this.t(1)
			nInsideTexCount += 1
    } else {
      outside_points(nOutsidePointCount) = this.p(1)
      nOutsidePointCount += 1
			outside_tex(nOutsideTexCount) = this.t(1)
			nOutsideTexCount += 1
    }
		if (d2 >= 0) {
      inside_points(nInsidePointCount) = this.p(2)
      nInsidePointCount += 1
			inside_tex(nInsideTexCount) = this.t(2)
			nInsideTexCount += 1
    } else {
      outside_points(nOutsidePointCount) = this.p(2)
      nOutsidePointCount += 1
			outside_tex(nOutsideTexCount) = this.t(2)
			nOutsideTexCount += 1
    }

		// Now classify triangle points, and break the input triangle into 
		// smaller output triangles if required. There are four possible
		// outcomes...

		if (nInsidePointCount == 0)
		{
			// All  points lie on the outside of plane, so clip whole triangle
			// It ceases to exist

			return (0, out_tri1, out_tri2) // No returned triangles are valid
		}

		if (nInsidePointCount == 3) 
		{
			// All points lie on the inside of plane, so do nothing
			// and allow the triangle to simply pass through
			out_tri1.p(0) = this.p(0)
      out_tri1.p(1) = this.p(1)
      out_tri1.p(2) = this.p(2)
			out_tri1.t(0) = this.t(0)
			out_tri1.t(1) = this.t(1)
			out_tri1.t(2) = this.t(2)
      out_tri1.bri = this.bri
			out_tri1.sat = this.sat
			out_tri1.col =  this.col

			return (1, out_tri1, out_tri2) // Just the one retuned original triangle is valid
		}

		if (nInsidePointCount == 1 && nOutsidePointCount == 2)
		{
			// Triangle should be clipped. As 
		  // two points lie outside
			// the plane, the triangle simply becomes a smaller triangle

			// Copy appearance info to new triangle
      out_tri1.bri = this.bri
			out_tri1.sat = this.sat
			out_tri1.col = this.col;

			// The inside point is valid, so keep that...
			out_tri1.p(0) = inside_points(0)
			out_tri1.t(0) = inside_tex(0)

			// but the two new points are at the locations where the 
			// original sides of the triangle (lines) intersect with the plane
			val ip1: (Vec3d, Double) = intersectPlane(plane_p, plane_n, inside_points(0), outside_points(0))
			val ip2: (Vec3d, Double) = intersectPlane(plane_p, plane_n, inside_points(0), outside_points(1))
			out_tri1.p(1) = ip1._1
			out_tri1.t(1).u = ip1._2 * (outside_tex(0).u - inside_tex(0).u) + inside_tex(0).u
			out_tri1.t(1).v = ip1._2 * (outside_tex(0).v - inside_tex(0).v) + inside_tex(0).v

			out_tri1.p(2) = ip2._1
			out_tri1.t(2).u = ip2._2 * (outside_tex(0).u - inside_tex(0).u) + inside_tex(0).u
			out_tri1.t(2).v = ip2._2 * (outside_tex(0).v - inside_tex(0).v) + inside_tex(0).v

			return (1, out_tri1, out_tri2) // Return the newly formed single triangle
		}

		if (nInsidePointCount == 2 && nOutsidePointCount == 1)
		{
			// Triangle should be clipped. As two points lie inside the plane,
			// the clipped triangle becomes a "quad". Fortunately, we can
			// represent a quad with two new triangles

			// Copy appearance info to new triangles
      out_tri1.bri = this.bri
			out_tri1.sat = this.sat
			out_tri1.col = this.col

      out_tri2.bri = this.bri
			out_tri2.sat = this.sat
			out_tri2.col = this.col

			// The first triangle consists of the two inside points and a new
			// point determined by the location where one side of the triangle
			// intersects with the plane
			out_tri1.p(0) = inside_points(0)
			out_tri1.p(1) = inside_points(1)
			out_tri1.t(0) = inside_tex(0)
			out_tri1.t(1) = inside_tex(1)

			val ip1: (Vec3d, Double) = intersectPlane(plane_p, plane_n, inside_points(0), outside_points(0))
			out_tri1.p(2) = ip1._1
			out_tri1.t(2).u = ip1._2 * (outside_tex(0).u - inside_tex(0).u) + inside_tex(0).u
			out_tri1.t(2).v = ip1._2 * (outside_tex(0).v - inside_tex(0).v) + inside_tex(0).v

			// The second triangle is composed of one of he inside points, a
			// new point determined by the intersection of the other side of the 
			// triangle and the plane, and the newly created point above
			out_tri2.p(0) = inside_points(1)
			out_tri2.p(1) = out_tri1.p(2)
			out_tri2.t(0) = inside_tex(1)
			out_tri2.t(1) = out_tri1.t(2)
			val ip2: (Vec3d, Double) = intersectPlane(plane_p, plane_n, inside_points(1), outside_points(0))
			out_tri2.p(2) = ip2._1
			out_tri1.t(2).u = ip2._2 * (outside_tex(0).u - inside_tex(1).u) + inside_tex(1).u
			out_tri1.t(2).v = ip2._2 * (outside_tex(0).v - inside_tex(1).v) + inside_tex(1).v

			return (2, out_tri1, out_tri2) // Return two newly formed triangles which form a quad
		}
    (1, out_tri1, out_tri2)
	}

	def swap[A](x: A, y: A): (A, A) = {
		(y, x)
	}

	def texturedTriangle(gc: GraphicsContext, sprite: Image): Unit = {
		var x1: Int = this.p(0).x.toInt; var y1: Int = this.p(0).y.toInt; var u1: Double = this.t(0).u; var v1: Double = this.t(0).v
		var x2: Int = this.p(1).x.toInt; var y2: Int = this.p(1).y.toInt; var u2: Double = this.t(1).u; var v2: Double = this.t(1).v
		var x3: Int = this.p(2).x.toInt; var y3: Int = this.p(2).y.toInt; var u3: Double = this.t(2).u; var v3: Double = this.t(2).v
		if(y2 < y1) {
			val tmp: (Int, Int) = swap(y1, y2)
			y1 = tmp._1
			y2 = tmp._2

			val tmp1: (Int, Int) = swap(x1, x2)
			x1 = tmp1._1
			x2 = tmp1._2

			val tmp2: (Double, Double) = swap(u1, u2)
			u1 = tmp2._1
			u2 = tmp2._2

			val tmp3: (Double, Double) = swap(v1, v2)
			v1 = tmp3._1
			v2 = tmp3._2
		}

		if(y3 < y1) {
			val tmp: (Int, Int) = swap(y1, y3)
			y1 = tmp._1
			y3 = tmp._2

			val tmp1: (Int, Int) = swap(x1, x3)
			x1 = tmp1._1
			x3 = tmp1._2

			val tmp2: (Double, Double) = swap(u1, u3)
			u1 = tmp2._1
			u3 = tmp2._2

			val tmp3: (Double, Double) = swap(v1, v3)
			v1 = tmp3._1
			v3 = tmp3._2
		}

		if(y3 < y2) {
			val tmp: (Int, Int) = swap(y2, y3)
			y2 = tmp._1
			y3 = tmp._2

			val tmp1: (Int, Int) = swap(x2, x3)
			x2 = tmp1._1
			x3 = tmp1._2

			val tmp2: (Double, Double) = swap(u2, u3)
			u2 = tmp2._1
			u3 = tmp2._2

			val tmp3: (Double, Double) = swap(v2, v3)
			v2 = tmp3._1
			v3 = tmp3._2
		}

		var dy1: Int = y2 - y1
		var dx1: Int = x2 - x1
		var dv1: Double = v2 - v1
		var du1: Double = u2 - u1

		var dy2: Int = y3 - y1
		var dx2: Int = x3 - x1
		var dv2: Double = v3 - v1
		var du2: Double = u3 - u1

		var tex_u: Double = 0.0
		var tex_v: Double = 0.0

		var dax_step: Double = 0.0
		var dbx_step: Double = 0.0
		var du1_step: Double = 0.0
		var dv1_step: Double = 0.0
		var du2_step: Double = 0.0
		var dv2_step: Double = 0.0

		if(dy1 != 0) dax_step = dx1 / math.abs(dy1).toDouble
		if(dy2 != 0) dbx_step = dx2 / math.abs(dy2).toDouble

		if(dy1 != 0) du1_step = du1 / math.abs(dy1).toDouble
		if(dy1 != 0) dv1_step = dv1 / math.abs(dy1).toDouble

		if(dy2 != 0) du2_step = du2 / math.abs(dy2).toDouble
		if(dy2 != 0) dv2_step = dv2 / math.abs(dy2).toDouble

		if(dy1 != 0) {
			for(i <- y1 to y2) {
				var ax: Int = (x1 + (i - y1).toDouble * dax_step).toInt
				var bx: Int = (x1 + (i - y1).toDouble * dbx_step).toInt

				var tex_su: Double = u1 + (i - y1).toDouble * du1_step
				var tex_sv: Double = v1 + (i - y1).toDouble * dv1_step

				var tex_eu: Double = u1 + (i - y1).toDouble * du2_step
				var tex_ev: Double = v1 + (i - y1).toDouble * dv2_step

				if(ax > bx) {
					val tmp: (Int, Int) = swap(ax, bx)
					ax = tmp._1
					bx = tmp._2

					val tmp1: (Double, Double) = swap(tex_su, tex_eu)
					tex_su = tmp1._1
					tex_eu = tmp1._2

					val tmp2: (Double, Double) = swap(tex_sv, tex_ev)
					tex_sv = tmp2._1
					tex_ev = tmp2._2
				}

				tex_u = tex_su
				tex_v = tex_sv

				val tstep: Double = 1.0 / (bx - ax).toDouble
				var t: Double = 0.0
				val pix = sprite.pixelReader
				val gcPixel = gc.pixelWriter

				for(j <- ax until bx) {
					tex_u = (1.0 - t) * tex_su + t * tex_eu
					tex_v = (1.0 - t) * tex_sv + t * tex_ev

					gcPixel.setColor(j, i, pix.get.getColor((tex_u * sprite.getWidth).toInt, (tex_v * sprite.getHeight).toInt))
					t += tstep
				}
			}

			dy1 = y3 - y2
			dx1 = x3 - x2
			dv1 = v3 - v2
			du1 = u3 - u2

			if(dy1 != 0) dax_step = dx1 / math.abs(dy1).toDouble
			if(dy2 != 0) dbx_step = dx2 / math.abs(dy2).toDouble

			du1_step = 0.0; dv1_step = 0.0
			if(dy1 != 0) du1_step = du1 / math.abs(dy1).toDouble
			if(dy1 != 0) dv1_step = dv1 / math.abs(dy1).toDouble

			if(dy1 != 0) {
				for(i <- y2 to y3) {
					var ax: Int = (x2 + (i - y2).toDouble * dax_step).toInt
					var bx: Int = (x1 + (i - y1).toDouble * dbx_step).toInt

					var tex_su: Double = u2 + (i - y2).toDouble * du1_step
					var tex_sv: Double = v2 + (i - y2).toDouble * dv1_step

					var tex_eu: Double = u1 + (i - y1).toDouble * du2_step
					var tex_ev: Double = v1 + (i - y1).toDouble * dv2_step

					if(ax > bx) {
						val tmp: (Int, Int) = swap(ax, bx)
						ax = tmp._1
						bx = tmp._2

						val tmp1: (Double, Double) = swap(tex_su, tex_eu)
						tex_su = tmp1._1
						tex_eu = tmp1._2

						val tmp2: (Double, Double) = swap(tex_sv, tex_ev)
						tex_sv = tmp2._1
						tex_ev = tmp2._2
					}

					tex_u = tex_su
					tex_v = tex_sv

					val tstep: Double = 1.0 / (bx - ax).toDouble
					var t: Double = 0.0
					val pix = sprite.pixelReader
					val gcPixel = gc.pixelWriter

					for(j <- ax until bx) {
						tex_u = (1.0 - t) * tex_su + t * tex_eu
						tex_v = (1.0 - t) * tex_sv + t * tex_ev

						gcPixel.setColor(j, i, pix.get.getColor((tex_u * sprite.getWidth).toInt, (tex_v * sprite.getHeight).toInt))
						t += tstep
					}
				}
			}
		}
	}
}