package graphicsengine3d

import scalafx.scene.paint.Color
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.canvas.Canvas

class Triangle {
  var p: Array[Vec3d] = Array.fill(3)(new Vec3d)
  var bri: Double = 1.0
  var col: Color = Color.Black
	var sat: Double = 1.0
  def apply(vec1: Vec3d, vec2: Vec3d, vec3: Vec3d): Unit = {
    this.p(0) = vec1; this.p(1) = vec2; this.p(2) = vec3
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
  
  def intersectPlane(planeP: Vec3d, plane_n: Vec3d, lineStart: Vec3d, lineEnd: Vec3d): Vec3d = {
    val planeN: Vec3d = plane_n.normalize
    val planeD: Double = -planeN.dotProduct(planeP)
    val ad: Double = lineStart.dotProduct(planeN)
    val bd: Double = lineEnd.dotProduct(planeN)
    val t: Double = (-planeD - ad) / (bd - ad)
    val lineStartToEnd: Vec3d = lineEnd - lineStart
    val lineToIntersect: Vec3d = lineStartToEnd * t
    return lineStart + lineToIntersect
  }

	override def toString: String = {
		"(" + this.p(0).x + ", " + this.p(0).y + ", " + this.p(0).z + ")\n" +
		"(" + this.p(1).x + ", " + this.p(1).y + ", " + this.p(1).z + ")\n" +
		"(" + this.p(2).x + ", " + this.p(2).y + ", " + this.p(2).z + ")"
	}

  def clipAgainstPlane(plane_p: Vec3d, planeN: Vec3d, out_tri1: Triangle, out_tri2: Triangle): Int = {
		// Make sure plane normal is indeed normal
		val plane_n: Vec3d = planeN.normalize

		// Return signed shortest distance from point to plane, plane normal must be normalised
		def dist(p: Vec3d) = {
			plane_n.x * p.x + plane_n.y * p.y + plane_n.z * p.z - plane_n.dotProduct(plane_p)
		}

		// Create two temporary storage arrays to classify points either side of plane
		// If distance sign is positive, point lies on "inside" of plane
		val inside_points: Array[Vec3d] = Array.fill(3)(new Vec3d);  var nInsidePointCount: Int = 0
		val outside_points: Array[Vec3d] = Array.fill(3)(new Vec3d); var nOutsidePointCount: Int = 0

		// Get signed distance of each point in triangle to plane
		val d0: Double = dist(this.p(0))
		val d1: Double = dist(this.p(1))
		val d2: Double = dist(this.p(2))

		if (d0 >= 0) {
      inside_points(nInsidePointCount) = this.p(0)
      nInsidePointCount += 1
    } else {
      outside_points(nOutsidePointCount) = this.p(0) 
      nOutsidePointCount += 1
    }
		if (d1 >= 0) {
      inside_points(nInsidePointCount) = this.p(1) 
      nInsidePointCount += 1
    } else {
      outside_points(nOutsidePointCount) = this.p(1)
      nOutsidePointCount += 1
    }
		if (d2 >= 0) {
      inside_points(nInsidePointCount) = this.p(2)
      nInsidePointCount += 1
    } else {
      outside_points(nOutsidePointCount) = this.p(2)
      nOutsidePointCount += 1
    }

		// Now classify triangle points, and break the input triangle into 
		// smaller output triangles if required. There are four possible
		// outcomes...

		if (nInsidePointCount == 0)
		{
			// All  points lie on the outside of plane, so clip whole triangle
			// It ceases to exist

			return 0 // No returned triangles are valid
		}

		if (nInsidePointCount == 3) 
		{
			// All points lie on the inside of plane, so do nothing
			// and allow the triangle to simply pass through
			out_tri1.p(0) = this.p(0)
      out_tri1.p(1) = this.p(1)
      out_tri1.p(2) = this.p(2)
      out_tri1.bri = this.bri
			out_tri1.sat = this.sat
			out_tri1.col =  this.col

			return 1 // Just the one retuned original triangle is valid
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

			// but the two new points are at the locations where the 
			// original sides of the triangle (lines) intersect with the plane
			out_tri1.p(1) = intersectPlane(plane_p, plane_n, inside_points(0), outside_points(0))
			out_tri1.p(2) = intersectPlane(plane_p, plane_n, inside_points(0), outside_points(1))

			return 1 // Return the newly formed single triangle
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
			out_tri1.p(2) = intersectPlane(plane_p, plane_n, inside_points(0), outside_points(0))

			// The second triangle is composed of one of he inside points, a
			// new point determined by the intersection of the other side of the 
			// triangle and the plane, and the newly created point above
			out_tri2.p(0) = inside_points(1)
			out_tri2.p(1) = out_tri1.p(2)
			out_tri2.p(2) = intersectPlane(plane_p, plane_n, inside_points(1), outside_points(0))

			return 2 // Return two newly formed triangles which form a quad
		}
    1
	}
}