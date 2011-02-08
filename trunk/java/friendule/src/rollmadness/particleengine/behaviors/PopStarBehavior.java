package rollmadness.particleengine.behaviors;

import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import java.util.logging.Level;
import java.util.logging.Logger;
import rollmadness.particleengine.ColorFunction;
import rollmadness.particleengine.ParticleBehavior;
import rollmadness.particleengine.QuaternionFunction;
import rollmadness.particleengine.TimeFunction;
import rollmadness.particleengine.Vector3fFunction;
import rollmadness.particleengine.functions.Gradient;
import rollmadness.particleengine.functions.GrowingScale;
import rollmadness.particleengine.functions.LifeTime;
import rollmadness.particleengine.functions.RandomDirectionTranslator;
import rollmadness.particleengine.functions.RandomRotation;

public class PopStarBehavior extends ParticleBehavior {
    public static final Object KEY_ORIGIN = 0;
    private final Vector3f ORIGIN = new Vector3f();
    private final TimeFunction LIFE = new LifeTime(1f);
    private final ColorFunction COLOR = new Gradient(ColorRGBA.Yellow, ColorRGBA.Red, 1f);
    private final RandomDirectionTranslator TRANSLATION;
    private final Vector3fFunction SCALE = new GrowingScale(2);
    private final RandomRotation ROT = new RandomRotation();

    public PopStarBehavior(float speed) {
	TRANSLATION = new RandomDirectionTranslator(ORIGIN, speed);
    }
//
//    @Override
//    public QuaternionFunction getRotation() {
//	return ROT;
//    }

    @Override
    public ColorFunction getColor() {
	return COLOR;
    }

    @Override
    public TimeFunction getLife() {
	return LIFE;
    }

    @Override
    public Vector3fFunction getScale() {
	return SCALE;
    }

    @Override
    public Vector3fFunction getTranslation() {
	return TRANSLATION;
    }

    @Override
    public ParticleBehavior setProperty(Object name, Object value) {
	if(KEY_ORIGIN.equals(name) && value instanceof Vector3f) {
	    ORIGIN.set((Vector3f)value);
	    TRANSLATION.randomize();
	    ROT.randomize();
	} else {
	    Logger.getLogger(getClass().getName()).log(Level.INFO, "Unrecognized property " + name + " with value " + value);
	}
	return this;
    }

}
