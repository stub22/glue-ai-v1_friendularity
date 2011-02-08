package rollmadness.util;

import com.jme3.bounding.BoundingBox;
import com.jme3.bounding.BoundingVolume;
import com.jme3.math.Vector3f;
import com.jme3.renderer.Camera;
import java.awt.geom.Rectangle2D;

public class Projections {

    public Rectangle2D projectBounds(BoundingVolume volume, Camera camera) {
	BoundingBox box = new BoundingBox(volume.getCenter().clone(), 0.1f, 0.1f, 0.1f);
	box.mergeLocal(volume);
	Vector3f v = camera.getScreenCoordinates(volume.getCenter());
	Rectangle2D.Float projection = new Rectangle2D.Float(v.x, v.y, 1, 1);
	box.getMin(v);
	v = camera.getScreenCoordinates(v);
	projection.add(v.x, v.y);
	v = camera.getScreenCoordinates(box.getMax(v));
	projection.add(v.x, v.y);
//	float dx = 2 * box.getXExtent();
//	float dy = 2 * box.getYExtent();
//	float dz = 2 * box.getZExtent();
//	Vector3f center = volume.getCenter();
//	Vector3f a = center.add(-box.getXExtent(), -box.getYExtent(), -box.getZExtent());
//	Vector3f b = a.add(dx, 0, 0);
//	Vector3f c = b.add(0, dy, 0);
//	Vector3f d = c.add(-dx, 0, 0);
//	Vector3f e = a.add(0, 0, dz);
//	Vector3f f = b.add(0, 0, dz);
//	Vector3f g = c.add(0, 0, dz);
//	Vector3f h = d.add(0, 0, dz);
//	for (Vector3f vector3f : new Vector3f[] { a, b, c, d, e, f, g, h }) {
//	    vector3f = camera.getScreenCoordinates(vector3f);
//	    projection.add(vector3f.x, vector3f.y);
//	}
	return projection;
    }
}
