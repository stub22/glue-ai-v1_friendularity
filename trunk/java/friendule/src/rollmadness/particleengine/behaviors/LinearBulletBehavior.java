package rollmadness.particleengine.behaviors;

import com.jme3.math.Vector3f;
import rollmadness.particleengine.ParticleBehavior;
import rollmadness.particleengine.TimeFunction;
import rollmadness.particleengine.Vector3fFunction;

public final class LinearBulletBehavior extends ParticleBehavior {
    public static final Object ORIGIN = 0;
    public static final Object DESTINATION = 1;
    public static final Object SPEED = 2;

    private final TimeFunction lifeFunction = new TimeFunction() {
	private float time = 0;

	public float apply(float fps) {
	    if(fps == START) {
		time = 0;
	    } else {
		time += fps;
	    }
	    return time >= flytime ? END : time;
	}
    };
    private final Vector3fFunction motion = new Vector3fFunction() {

	public Vector3f apply(Vector3f data, float time, float delta) {
	    float ds = time * speed;
	    Vector3f displacement = dest.subtract(origin).normalizeLocal().multLocal(ds);
	    data.set(origin).addLocal(displacement);
	    return data;
	}
    };
    private final Vector3f origin = new Vector3f();
    private final Vector3f dest = new Vector3f();
    private float speed = 1;
    private float flytime;

    public void set(Vector3f origin, Vector3f dest, float speed) {
	this.origin.set(origin);
	this.dest.set(dest);
	this.speed = speed;
	flytime = origin.distance(dest) / speed;
    }

    private float signum(float a) {
	float s = Math.signum(a);
	if(s == 0) s = 1;
	return s;
    }

    @Override
    public TimeFunction getLife() {
	return lifeFunction;
    }

    @Override
    public Vector3fFunction getTranslation() {
	return motion;
    }

    @Override
    public ParticleBehavior setProperty(Object name, Object value) {
	if(name == ORIGIN && value instanceof Vector3f) {
	    origin.set((Vector3f) value);
	    set(origin, dest, speed);
	} else if(name == DESTINATION  && value instanceof Vector3f) {
	    dest.set((Vector3f) value);
	    set(origin, dest, speed);
	} else if(name == SPEED && value instanceof Number) {
	    set(origin, dest, ((Number) value).floatValue());
	}
	return this;
    }
}
