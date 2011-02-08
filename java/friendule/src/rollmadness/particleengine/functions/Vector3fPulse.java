package rollmadness.particleengine.functions;

import com.jme3.math.Vector3f;
import rollmadness.particleengine.Vector3fFunction;

public class Vector3fPulse extends Pulse implements Vector3fFunction {

    private Vector3f min, max;
    private float length, dx, dy, dz;
    private float dir = 1;
    private float timesub;

    public Vector3fPulse(Vector3f min, Vector3f max, float len) {
	this.min = min;
	this.max = max;
	length = len;
	dx = max.x - min.x;
	dy = max.y - min.y;
	dz = max.z - min.z;
    }

    public Vector3f apply(Vector3f data, float time, float delta) {
	float a = pulse(time, length);
	if(a < 0) a = -a;
	data.set(min.x + dx * a, min.y + dy * a, min.z + dz * a);
	return data;
    }
}
