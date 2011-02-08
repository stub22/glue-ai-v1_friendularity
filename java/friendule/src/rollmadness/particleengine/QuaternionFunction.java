package rollmadness.particleengine;

import com.jme3.math.Quaternion;

public interface QuaternionFunction {
    QuaternionFunction IDENTITY = new QuaternionFunction() {

	public Quaternion apply(Quaternion q, float time, float delta) {
	    return q;
	}
    };

    Quaternion apply(Quaternion q, float time, float delta);
}
