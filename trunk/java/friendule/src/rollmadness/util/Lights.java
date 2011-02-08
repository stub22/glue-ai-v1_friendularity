package rollmadness.util;

import com.jme3.light.DirectionalLight;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import java.awt.Color;

public class Lights {

    public DirectionalLight createDirectionalLight(Node node,
	    Vector3f direction, Color color) {
	DirectionalLight dl = new DirectionalLight();
	dl.setColor(new ColorRGBA(
		color.getRed() / 255f,
		color.getGreen() / 255f,
		color.getBlue() / 255f,
		color.getAlpha() / 255f));
	dl.setDirection(direction.normalize());
	node.addLight(dl);
	return dl;
    }

    public DirectionalLight createDirectionalLight(Node node,
	    Vector3f direction, ColorRGBA color) {
	DirectionalLight dl = new DirectionalLight();
	dl.setColor(color.clone());
	dl.setDirection(direction.normalize());
	node.addLight(dl);
	return dl;
    }
}
