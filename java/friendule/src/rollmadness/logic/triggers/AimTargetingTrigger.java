package rollmadness.logic.triggers;

import com.jme3.bounding.BoundingVolume;
import com.jme3.math.Ray;
import com.jme3.renderer.Camera;
import com.jme3.scene.Spatial;
import java.util.List;
import jme3clogic.Condition;
import jme3clogic.Reaction;
import jme3clogic.Trigger;
import rollmadness.stages.PlayerHud;

public class AimTargetingTrigger extends Trigger {
    private final Ray ray = new Ray();

    public AimTargetingTrigger(final Camera camera, final List<Spatial> targets,
	    final PlayerHud hud) {
	this.condition = new Condition() {

	    public boolean holds(float timePerFrame) {
		boolean holds = false;
		ray.setDirection(camera.getDirection().normalize());
		ray.setOrigin(camera.getLocation());
		for (Spatial target : targets) {
		    target.updateGeometricState();
		    target.updateModelBound();
		    BoundingVolume vol = target.getWorldBound();
		    if(vol.intersects(ray) && vol.getCenter().distance(
			    camera.getLocation()) <= hud.getWeaponRange()) {
			holds = true;
			break;
		    }
		}
		if(!holds) {
		    hud.set(PlayerHud.AIM_STATE, PlayerHud.AIM_WAIT);
		} else {
		    hud.set(PlayerHud.AIM_STATE, PlayerHud.AIM_TARGET);
		}
		return holds;
	    }
	};
	this.reaction = Reaction.NOTHING;
    }
}
