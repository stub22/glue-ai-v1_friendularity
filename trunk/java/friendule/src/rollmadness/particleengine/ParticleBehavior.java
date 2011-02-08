package rollmadness.particleengine;

public abstract class ParticleBehavior {
    public static final ParticleBehavior IDENTITY = new ParticleBehavior() {

	@Override
	public ParticleBehavior setProperty(Object name, Object value) {
	    return this;
	}
    };

    private TimeFunction life = TimeFunction.IDENTITY;
    private ColorFunction color = ColorFunction.IDENTITY;
    private Vector3fFunction translation = Vector3fFunction.IDENTITY;
    private Vector3fFunction scale = Vector3fFunction.IDENTITY;
    private QuaternionFunction rotation = QuaternionFunction.IDENTITY;

    public abstract ParticleBehavior setProperty(Object name, Object value);

    public ParticleBehavior setLife(TimeFunction t) {
	life = t;
	return this;
    }

    public ParticleBehavior setColor(ColorFunction c) {
	color = c;
	return this;
    }

    public ParticleBehavior setTranslation(Vector3fFunction f) {
	translation = f;
	return this;
    }

    public ParticleBehavior setScale(Vector3fFunction f) {
	scale = f;
	return this;
    }

    public ParticleBehavior setRotation(QuaternionFunction f) {
	rotation = f;
	return this;
    }

    public TimeFunction getLife() {
	return life;
    }

    public ColorFunction getColor() {
	return color;
    }

    public Vector3fFunction getTranslation() {
	return translation;
    }

    public Vector3fFunction getScale() {
	return scale;
    }

    public QuaternionFunction getRotation() {
	return rotation;
    }
}
