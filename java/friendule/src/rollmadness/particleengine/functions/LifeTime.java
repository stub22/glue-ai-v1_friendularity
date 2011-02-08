package rollmadness.particleengine.functions;

import rollmadness.particleengine.TimeFunction;

public class LifeTime implements TimeFunction {
    private float timeline;
    private final float threshold;

    public LifeTime(float seconds) {
	threshold = seconds;
    }

    public float apply(float fps) {
	if(fps == TimeFunction.START) {
	    timeline = 0;
	} else {
	    timeline += fps;
	}
	return timeline > threshold ? END : timeline;
    }

}
