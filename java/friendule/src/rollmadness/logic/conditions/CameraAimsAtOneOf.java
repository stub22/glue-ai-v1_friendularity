package rollmadness.logic.conditions;

import com.jme3.bounding.BoundingVolume;
import com.jme3.math.Ray;
import com.jme3.renderer.Camera;
import com.jme3.scene.Spatial;
import java.util.List;
import jme3clogic.Condition;

public class CameraAimsAtOneOf extends Condition {
    private final Camera camera;
    private final List<Spatial> targets;
    private final Ray ray = new Ray();

    public CameraAimsAtOneOf(Camera cam, List<Spatial> targets) {
	camera = cam;
	this.targets = targets;
    }

    @Override
    public boolean holds(float timePerFrame) {
	ray.setDirection(camera.getDirection().normalize());
	ray.setOrigin(camera.getLocation());
	for (Spatial target : targets) {
	    target.updateGeometricState();
	    target.updateModelBound();
	    BoundingVolume vol = target.getWorldBound();
	    if(vol.intersects(ray)) {
		return true;
	    }
	}
	return false;
    }

}
