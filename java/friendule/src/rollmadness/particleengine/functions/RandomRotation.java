package rollmadness.particleengine.functions;

import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import java.util.Random;
import rollmadness.particleengine.QuaternionFunction;

public class RandomRotation implements QuaternionFunction {
    private final static Random RANDOM = new Random();
    private float x = RANDOM.nextFloat();
    private float y = RANDOM.nextFloat();
    private float z = RANDOM.nextFloat();
    private final float[] a = new float[3];

    public void randomize() {
	x = RANDOM.nextFloat();
	y = RANDOM.nextFloat();
	z = RANDOM.nextFloat();
    }

    public Quaternion apply(Quaternion q, float time, float delta) {
	float d = time * FastMath.PI / 200;
	q.toAngles(a);
	a[1] += d * y;
	a[0] = 0;
	a[2] = 0;
	q.fromAngles(a);
	return q;
    }

}
