package rollmadness.particleengine;

public interface TimeFunction {
    float START = -1;
    float END = -2;

    TimeFunction IDENTITY = new TimeFunction() {
	public float apply(float fps) { return fps; }
    };

    float apply(float fps);
}
