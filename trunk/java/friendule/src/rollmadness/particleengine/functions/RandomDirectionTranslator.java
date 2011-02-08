package rollmadness.particleengine.functions;

import com.jme3.math.Vector3f;
import java.util.Random;
import rollmadness.particleengine.Vector3fFunction;

public class RandomDirectionTranslator implements Vector3fFunction {
    private static final Random RANDOM = new Random();
    private final Vector3f DIRECTION = new Vector3f(
	    1 - 2 * RANDOM.nextFloat(),
	    1 - 2 * RANDOM.nextFloat(),
	    1 - 2 * RANDOM.nextFloat()).normalizeLocal();
    private final float SPEED;
    private final Vector3f ORIGIN;

    public RandomDirectionTranslator(Vector3f origin, float speed) {
	SPEED = speed;
	ORIGIN = origin;
    }

    public void randomize() {
	DIRECTION.set(
		1 - 2 * RANDOM.nextFloat(),
		1 - 2 * RANDOM.nextFloat(),
		1 - 2 * RANDOM.nextFloat()).normalizeLocal();
    }

    public Vector3f apply(Vector3f data, float time, float delta) {
        data.set(DIRECTION.mult(SPEED * time).add(ORIGIN));
	return data;
    }
}
