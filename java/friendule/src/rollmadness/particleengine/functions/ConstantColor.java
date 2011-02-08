package rollmadness.particleengine.functions;

import com.jme3.math.ColorRGBA;
import rollmadness.particleengine.ColorFunction;

public class ConstantColor implements ColorFunction {
    private final ColorRGBA color;

    public ConstantColor(ColorRGBA color) {
	this.color = new ColorRGBA(color);
    }

    public ColorRGBA apply(ColorRGBA color, float time, float delta) {
	color.set(this.color);
	return color;
    }

}
