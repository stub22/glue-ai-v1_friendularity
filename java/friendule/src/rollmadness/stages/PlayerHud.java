package rollmadness.stages;

import com.jme3.scene.Geometry;
import com.jme3.scene.Spatial.CullHint;
import com.jme3.ui.Picture;
import java.awt.Color;
import java.awt.Dimension;
import javax.swing.JTextArea;
import javax.swing.border.EmptyBorder;
import rollmadness.gameproperties.PropertyKey;
import rollmadness.gamestage.GameStage;
import rollmadness.gamestage.GameStageEnvironment;
import rollmadness.util.Pictures;
import rollmadness.util.image.PaintableTexture;
//mencoder -idx your_input.ogg -vf scale=640:480 -ovc lavc -oac mp3lame -o your_output.avi
/**
 * TODO realize this
 * @author pgi
 */
public class PlayerHud extends GameStage implements jme3clogic.basic.NumericValueHolder {
    public static final String AMMO = "ammo";
    public static final String ENERGY = "energy";
    public static final String SPEED = "speed";
    public static final String DESTROYED_TARGETS_COUNT = "targetcount";
    public static final String AIM_STATE = "aimstate";

    public static final int AIM_TARGET = 0;
    public static final int AIM_WAIT = 1;
    public static final int AIM_FIRE = 2;
    
    private int ammoCount = 100;
    private int energyCount = 100;
    private int speed = 0;
    private int destroyedTargetsCount = 0;
    private int totalTargetCount = 0;
    private final PaintableTexture SCREEN;
    private final JTextArea TEXT = new JTextArea();
    private final Picture aimNoTarget;
    private final Picture aimTarget;
    private final Picture aimFiring;

    public PlayerHud(GameStageEnvironment env) {
	super(env, "PlayerHud");
	aimNoTarget = new Pictures().newPicture(env.getAssetManager(),
		"rollmadness/textures/aim_notarget.png");
	aimTarget = new Pictures().newPicture(env.getAssetManager(),
		"rollmadness/textures/aim_target.png");
	aimFiring = new Pictures().newPicture(env.getAssetManager(),
		"rollmadness/textures/aim_firing.png");
	SCREEN = new PaintableTexture(env.getAssetManager(), 128, 128);
	TEXT.setOpaque(true);
	TEXT.setBackground(Color.BLACK);
	TEXT.setBounds(0, 0, SCREEN.getWidth(), SCREEN.getHeight());
	TEXT.setSize(new Dimension(SCREEN.getWidth(), SCREEN.getHeight()));
	TEXT.setPreferredSize(new Dimension(SCREEN.getWidth(), SCREEN.getHeight()));
	TEXT.getFont().deriveFont(9f);
	TEXT.setForeground(Color.WHITE);
	TEXT.setBorder(new EmptyBorder(2, 2, 2, 2));
	updateText();
    }

    public void setAimState(Number s) {
	switch(s.intValue()) {
	    case AIM_WAIT:
		aimNoTarget.setCullHint(CullHint.Never);
		aimTarget.setCullHint(CullHint.Always);
		aimFiring.setCullHint(CullHint.Always);
		break;
	    case AIM_TARGET:
		aimNoTarget.setCullHint(CullHint.Always);
		aimTarget.setCullHint(CullHint.Never);
		aimFiring.setCullHint(CullHint.Always);
		break;
	    case AIM_FIRE:
		aimNoTarget.setCullHint(CullHint.Always);
		aimTarget.setCullHint(CullHint.Always);
		aimFiring.setCullHint(CullHint.Never);
		break;
	}
    }

    @Override
    public void start() {
	Dimension screenSize = getGameStageEnvironment().getScreenSize();
	aimNoTarget.setLocalTranslation(screenSize.width / 2 - 32, screenSize.height / 2 - 32, 0);
	aimTarget.setLocalTranslation(screenSize.width / 2 - 32, screenSize.height / 2 - 32, 0);
	aimFiring.setLocalTranslation(screenSize.width / 2 - 32, screenSize.height / 2 - 32, 0);
	getGameStageEnvironment().getGuiNode().attachChild(aimNoTarget);
	getGameStageEnvironment().getGuiNode().attachChild(aimTarget);
	getGameStageEnvironment().getGuiNode().attachChild(aimFiring);
	setAimState(AIM_WAIT);
    }

    @Override
    public void pause() {
    }

    @Override
    public void stop() {
	aimNoTarget.removeFromParent();
	aimTarget.removeFromParent();
	aimFiring.removeFromParent();
    }

    public void setCapsuleDisplay(Geometry g) {
	g.setMaterial(SCREEN.getMaterial());
    }

    public void set(String field, Number value) {
	if(field.equals(AMMO)) {
	    ammoCount = value.intValue();
	} else if(field.equals(ENERGY)) {
	    energyCount = value.intValue();
	} else if(field.equals(SPEED)) {
	    speed = value.intValue();
	} else if(field.equals(DESTROYED_TARGETS_COUNT)) {
	    destroyedTargetsCount = value.intValue();
	} else if(field.equals(AIM_STATE)) {
	    setAimState(value);
	}
	updateText();
    }

    public Number get(String field) {
	if(field.equals(AMMO)) {
	    return ammoCount;
	} else if(field.equals(ENERGY)) {
	    return energyCount;
	} else if(field.equals(DESTROYED_TARGETS_COUNT)) {
	    return destroyedTargetsCount;
	} else if(field.equals(AIM_STATE)) {
	    return
		    aimFiring.getCullHint() == CullHint.Never ? AIM_FIRE :
		    aimNoTarget.getCullHint() == CullHint.Never  ? AIM_WAIT :
		    aimTarget.getCullHint() == CullHint.Never ? AIM_TARGET :
		    0;
	} else {
	    return 0;
	}
    }

    private void updateText() {
	TEXT.setText(
		"Ammo: " + ammoCount +
		"%\nEnergy: " + energyCount +
		"%\nSpeed: " + speed + " m/s\n" +
		"Hit Targets: " + destroyedTargetsCount + "/" + totalTargetCount);
	TEXT.print(SCREEN.getGraphics());
	SCREEN.updateTexture();
    }

    public void setTotalTargetCount(int size) {
	totalTargetCount = size;
	updateText();
    }

    public float getWeaponRange() {
	return getGameStageEnvironment().getGlobalProperty(PropertyKey.BULLET_RANGE, Number.class).floatValue();
    }
}
