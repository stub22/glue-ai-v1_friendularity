package rollmadness.particleengine.functions;

import com.jme3.math.ColorRGBA;
import rollmadness.particleengine.ColorFunction;

public class Gradient implements ColorFunction {
    private final ColorRGBA start, stop;
    private final float dr, dg, db, da;
    private final float length;

    public Gradient(ColorRGBA start, ColorRGBA stop, float length) {
	this.start = start;
	this.stop = stop;
	this.length = Math.abs(length);
	dr = (stop.r - start.r);
	dg = (stop.g - start.g);
	db = (stop.b - start.b);
	da = (stop.a - start.a);
    }

    public ColorRGBA apply(ColorRGBA color, float time, float delta) {
	float alpha = time;//Math.min(time / length, 1);
	color.r = start.r + alpha * dr;
	color.g = start.g + alpha * dg;
	color.b = start.b + alpha * db;
	color.a = start.a + alpha * da;
	color.r = Math.min(color.r, 1);
	color.r = Math.max(color.r, 0);
	color.a = Math.min(color.a, 1);
	color.a = Math.max(color.a, 0);
	color.b = Math.min(color.b, 1);
	color.b = Math.max(color.b, 0);
	color.g = Math.min(color.g, 1);
	color.g = Math.max(color.g, 0);
	return color;
    }

}
