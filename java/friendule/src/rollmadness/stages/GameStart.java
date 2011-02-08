package rollmadness.stages;

import com.jme3.input.KeyInput;
import com.jme3.scene.Spatial.CullHint;
import com.jme3.texture.Texture;
import com.jme3.texture.Texture2D;
import com.jme3.ui.Picture;
import java.awt.Dimension;
import java.util.logging.Level;
import java.util.logging.Logger;
import jme3clogic.Condition;
import jme3clogic.Reaction;
import jme3clogic.Trigger;
import jme3clogic.basic.conditions.ElapsedTime;
import jme3clogic.basic.conditions.KeyReleased;
import jme3clogic.basic.reactions.DetachSpatial;
import jme3clogic.basic.reactions.RemoveTrigger;
import rollmadness.formats.TrackInfo;
import rollmadness.gameproperties.PropertyKey;
import rollmadness.gamestage.GameStage;
import rollmadness.gamestage.GameStageEnvironment;

public class GameStart extends GameStage {
    private final Picture START_PICTURE;

    public GameStart(GameStageEnvironment env) {
	super(env, GameStart.class.getName());
	env.setGlobalProperty(PropertyKey.MAX_PLAYER_ENERGY, 100);
	env.setGlobalProperty(PropertyKey.MAX_PLAYER_AMMO, 100);
	env.setGlobalProperty(PropertyKey.BULLET_RANGE, 100);
	env.setGlobalProperty(PropertyKey.BULLET_SPEED, 75);
	env.setGlobalProperty(PropertyKey.BOOSTER_ENERGY, 1);
	env.setGlobalProperty(PropertyKey.BOOSTER_STRENGTH, 0.5f);
	START_PICTURE = new Picture("start label");
	Texture texture = env.getAssetManager().loadTexture(
		"rollmadness/textures/spacetostart.png");
	START_PICTURE.setTexture(env.getAssetManager(), (Texture2D) texture, true);
	START_PICTURE.setWidth(texture.getImage().getWidth());
	START_PICTURE.setHeight(texture.getImage().getHeight());
	Dimension screenSize = env.getScreenSize();
	float x = (screenSize.width - texture.getImage().getWidth()) / 2;
	float y = (screenSize.height - texture.getImage().getHeight()) / 2;
	START_PICTURE.setLocalTranslation(x, y, 0);
    }

    @Override
    public void start() {
	PlayerHud hud = (PlayerHud) findStage("PlayerHud");
	hud.set(PlayerHud.AMMO, getGameStageEnvironment().getGlobalProperty(
		PropertyKey.MAX_PLAYER_AMMO, Number.class));
	hud.set(PlayerHud.ENERGY, getGameStageEnvironment().getGlobalProperty(
		PropertyKey.MAX_PLAYER_ENERGY, Number.class));
	hud.set(PlayerHud.SPEED, 0);
	hud.set(PlayerHud.DESTROYED_TARGETS_COUNT, 0);

	getGameStageEnvironment().getGuiNode().attachChild(START_PICTURE);

	final TrackStage trackStage = (TrackStage) findStage("TrackStage");
	TrackInfo nextTrack = getGameStageEnvironment().getGlobalProperty(
		PropertyKey.NEXT_TRACK_INFO, TrackInfo.class);
	if(nextTrack != null) {
	    trackStage.playTrack(nextTrack);
	} else {
	    Logger.getLogger(getClass().getName()).log(Level.SEVERE,
		    "Game Start has not next track to start!!!");
	}
	trackStage.start();
	trackStage.pause();

	Trigger flashTrigger = new Trigger();
	flashTrigger.set(new ElapsedTime(1f), new Reaction() {

	    @Override
	    public void act(float timePerFrame) {
		if(START_PICTURE.getCullHint() != CullHint.Always) {
		    START_PICTURE.setCullHint(CullHint.Always);
		} else {
		    START_PICTURE.setCullHint(CullHint.Never);
		}
	    }
	});
	getGameStageEnvironment().getTriggerSystem().addTrigger(flashTrigger);

	Trigger startTrigger = new Trigger();
	Condition spaceReleased = new KeyReleased(getGameStageEnvironment().
		getInputManager(), KeyInput.KEY_SPACE);
	Reaction startTrack = new Reaction() {

	    @Override
	    public void act(float timePerFrame) {
		trackStage.start();
	    }
	};
	startTrigger.set(spaceReleased, 
		new RemoveTrigger(flashTrigger).and(
		new RemoveTrigger(startTrigger)).and(
		new DetachSpatial(START_PICTURE)).and(
		startTrack));
	getGameStageEnvironment().getTriggerSystem().addTrigger(startTrigger);
    }

    @Override
    public void pause() {
    }

    @Override
    public void stop() {
	START_PICTURE.removeFromParent();
    }
}
