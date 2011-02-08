package rollmadness.particleengine;

import com.jme3.math.ColorRGBA;

public interface ColorFunction {
    ColorFunction IDENTITY = new ColorFunction() {

	public ColorRGBA apply(ColorRGBA color, float time, float delta) {
	    return color;
	}
    };

    ColorRGBA apply(ColorRGBA color, float time, float delta);
}
