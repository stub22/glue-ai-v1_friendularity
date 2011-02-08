package rollmadness.logic.triggers;

import com.jme3.audio.AudioNode;
import com.jme3.scene.Spatial;
import jme3clogic.Trigger;
import jme3clogic.basic.Threshold;
import jme3clogic.basic.conditions.SpatialsAreNear;
import jme3clogic.basic.reactions.DetachSpatial;
import jme3clogic.basic.reactions.IncreaseNumericField;
import jme3clogic.basic.reactions.PlaySound;
import jme3clogic.basic.reactions.RemoveTrigger;
import rollmadness.stages.PlayerHud;

public class PickUpEnergy extends Trigger {

    public PickUpEnergy(Spatial player, Spatial energyBonus, PlayerHud hud, AudioNode pickupEnergySound) {
	condition = new SpatialsAreNear(player, energyBonus, new Threshold(4));
	reaction =
		new PlaySound(pickupEnergySound, hud.getGameStageEnvironment().getAudioRenderer()).and(
		new DetachSpatial(energyBonus)).and(
		new IncreaseNumericField(hud, PlayerHud.ENERGY, 5)).and(
		new RemoveTrigger(this));

    }
}
