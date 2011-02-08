package rollmadness.logic.triggers;

import com.jme3.bounding.BoundingVolume;
import com.jme3.scene.Spatial;
import jme3clogic.Trigger;
import jme3clogic.basic.MethodOwner;
import jme3clogic.basic.conditions.ElapsedTime;
import jme3clogic.basic.conditions.SpatialOutOfBound;
import jme3clogic.basic.reactions.CallMethod;
import jme3clogic.basic.reactions.Debug;
import jme3clogic.basic.reactions.Delay;
import jme3clogic.basic.reactions.RemoveTrigger;

public class WinTrigger extends Trigger {

    public WinTrigger(final Spatial player, final BoundingVolume goal,
	    final MethodOwner mo, final Trigger gameOver) {
	this.condition = new SpatialOutOfBound(goal, player).negate().
		repeatAfter(new ElapsedTime(2f));
	this.reaction = 
		new Debug("won").and(new RemoveTrigger(gameOver)).
		and(new CallMethod(mo, "popupWinLabel")).
		then(new Delay(5000L)).
		then(new CallMethod(mo, "jumpToWinStage"));
    }

}
