package rollmadness.logic.triggers;

import com.jme3.bullet.nodes.PhysicsNode;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
import jme3clogic.Reaction;
import jme3clogic.Trigger;
import jme3clogic.basic.Threshold;
import jme3clogic.basic.conditions.SpatialsAreNear;
import jme3clogic.basic.reactions.DetachSpatial;
import jme3clogic.basic.reactions.RemoveTrigger;

public class PickUpSprinterTrigger extends Trigger {

    public PickUpSprinterTrigger(final PhysicsNode player, Spatial sprinter) {
	final Vector3f sprintDirection = sprinter.getLocalRotation().
		multLocal(new Vector3f(0, 0, 50));
	condition = new SpatialsAreNear(player, sprinter, new Threshold(4));
	reaction = new Reaction() {

	    @Override
	    public void act(float timePerFrame) {
		player.setLinearVelocity(player.getLinearVelocity().
			addLocal(sprintDirection));
	    }
	}.and(new RemoveTrigger(this)).and(
		new DetachSpatial(sprinter));
    }

}
