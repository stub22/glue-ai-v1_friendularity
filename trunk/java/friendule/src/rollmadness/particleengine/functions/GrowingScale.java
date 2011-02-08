package rollmadness.particleengine.functions;

import com.jme3.math.Vector3f;
import rollmadness.particleengine.Vector3fFunction;

public class GrowingScale implements Vector3fFunction {
    private final float MAX;

    public GrowingScale(float max) {
	MAX = max;
    }

    public Vector3f apply(Vector3f data, float time, float delta) {
	float f = time * MAX;
	data.set(f, f, f);
	return data;
    }

}
