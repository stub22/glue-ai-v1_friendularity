package rollmadness.particleengine.functions;

import com.jme3.math.FastMath;

public abstract class Pulse {

    public float pulse(float time, float range) {
	return FastMath.sin(FastMath.HALF_PI * time / range);
    }
}
