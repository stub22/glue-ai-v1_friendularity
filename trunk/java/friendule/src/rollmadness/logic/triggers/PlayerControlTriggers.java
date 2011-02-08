package rollmadness.logic.triggers;

import com.jme3.audio.AudioNode;
import com.jme3.bullet.nodes.PhysicsNode;
import com.jme3.input.InputManager;
import com.jme3.input.KeyInput;
import com.jme3.math.Vector3f;
import com.jme3.renderer.Camera;
import jme3clogic.Condition;
import jme3clogic.Reaction;
import jme3clogic.Trigger;
import jme3clogic.basic.conditions.ElapsedTime;
import jme3clogic.basic.conditions.KeyPressed;
import jme3clogic.basic.reactions.PlaySound;
import rollmadness.gameproperties.PropertyKey;
import rollmadness.gamestage.GameStageEnvironment;
import rollmadness.logic.conditions.KeyPressedNoRepeat;
import rollmadness.stages.PlayerHud;

public class PlayerControlTriggers {
    private final Trigger strafeLeft, strafeRight, moveForth, moveBack;

    public PlayerControlTriggers(final PhysicsNode player, final PlayerHud hud,
	    final GameStageEnvironment env, AudioNode boosterSound) {

	Condition hasEnergy = new Condition() {

	    @Override
	    public boolean holds(float timePerFrame) {
		int availableEnergy = hud.get(PlayerHud.ENERGY).intValue();
		int requiredEnergy = env.getGlobalProperty(
			PropertyKey.BOOSTER_ENERGY, Number.class).intValue();
		return availableEnergy >= requiredEnergy;
	    }
	};
	Condition aPressed = new KeyPressedNoRepeat(env.getInputManager(), KeyInput.KEY_A);
	Condition dPressed = new KeyPressedNoRepeat(env.getInputManager(), KeyInput.KEY_D);
	Condition wPressed = new KeyPressedNoRepeat(env.getInputManager(), KeyInput.KEY_W);
	Condition sPressed = new KeyPressedNoRepeat(env.getInputManager(), KeyInput.KEY_S);
	
	aPressed = new KeyPressed(env.getInputManager(), KeyInput.KEY_A).repeatAfter(new ElapsedTime(0.1f));
	dPressed = new KeyPressed(env.getInputManager(), KeyInput.KEY_D).repeatAfter(new ElapsedTime(0.1f));
	wPressed = new KeyPressed(env.getInputManager(), KeyInput.KEY_W).repeatAfter(new ElapsedTime(0.1f));
	sPressed = new KeyPressed(env.getInputManager(), KeyInput.KEY_S).repeatAfter(new ElapsedTime(0.1f));

	Reaction reduceEnergy = new Reaction() {

	    @Override
	    public void act(float timePerFrame) {
		int used = env.getGlobalProperty(PropertyKey.BOOSTER_ENERGY,
			Number.class).intValue();
		int energy = hud.get(PlayerHud.ENERGY).intValue();
		energy = Math.max(0, energy - used);
		hud.set(PlayerHud.ENERGY, energy);
	    }
	};
	Reaction playBoosterSound = new PlaySound(boosterSound, env.getAudioRenderer());
	Reaction left = new Reaction() {

	    @Override
	    public void act(float timePerFrame) {
		final Camera camera = env.getCamera();
		final float boosterStrength = env.getGlobalProperty(
			PropertyKey.BOOSTER_STRENGTH, Number.class).floatValue();
		final Vector3f punch = camera.getLeft().mult(boosterStrength);
		player.setLinearVelocity(player.getLinearVelocity().addLocal(punch));
	    }
	};
	Reaction right = new Reaction() {

	    @Override
	    public void act(float timePerFrame) {
		final Camera camera = env.getCamera();
		final float boosterStrength = env.getGlobalProperty(
			PropertyKey.BOOSTER_STRENGTH, Number.class).floatValue();
		final Vector3f punch = camera.getLeft().negate().mult(boosterStrength);
		player.setLinearVelocity(player.getLinearVelocity().addLocal(punch));
	    }
	};
	Reaction forth = new Reaction() {

	    @Override
	    public void act(float timePerFrame) {
		final Camera camera = env.getCamera();
		final float boosterStrength = env.getGlobalProperty(
			PropertyKey.BOOSTER_STRENGTH, Number.class).floatValue();
		final Vector3f punch = camera.getDirection().mult(boosterStrength);
		player.setLinearVelocity(player.getLinearVelocity().addLocal(punch));
	    }
	};
	Reaction back = new Reaction() {

	    @Override
	    public void act(float timePerFrame) {
		final Camera camera = env.getCamera();
		final float boosterStrength = env.getGlobalProperty(
			PropertyKey.BOOSTER_STRENGTH, Number.class).floatValue();
		final Vector3f punch = camera.getDirection().negate().mult(boosterStrength);
		player.setLinearVelocity(player.getLinearVelocity().addLocal(punch));
	    }
	};

	this.strafeLeft = new Trigger(aPressed.and(hasEnergy),
		playBoosterSound.and(reduceEnergy).and(left));
	this.strafeRight = new Trigger(dPressed.and(hasEnergy),
		playBoosterSound.and(reduceEnergy).and(right));
	this.moveForth = new Trigger(wPressed.and(hasEnergy),
		playBoosterSound.and(reduceEnergy).and(forth));
	this.moveBack = new Trigger(sPressed.and(hasEnergy),
		playBoosterSound.and(reduceEnergy).and(back));
    }

    public Trigger[] getTriggers() {
	return new Trigger[] { strafeLeft, strafeRight, moveForth, moveBack };
    }
}
