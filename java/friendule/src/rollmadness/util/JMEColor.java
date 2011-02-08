package rollmadness.util;

import com.jme3.math.ColorRGBA;
import java.awt.Color;

public class JMEColor {
    private final ColorRGBA color;

    public JMEColor(Color c) {
	color = new ColorRGBA(c.getRed() / 255f, c.getGreen() / 255f, c.getBlue() / 255f, c.getAlpha() / 255f);
    }

    public ColorRGBA get() {
	return color;
    }
}
