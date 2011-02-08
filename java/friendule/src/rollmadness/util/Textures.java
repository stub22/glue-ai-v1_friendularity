package rollmadness.util;

import com.jme3.texture.Image;
import com.jme3.texture.Texture2D;
import com.jme3.util.BufferUtils;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.nio.ByteBuffer;
import jme3tools.converters.ImageToAwt;

public class Textures {

    public Texture2D createTexture(Color color, int width, int height) {
	BufferedImage buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
	Graphics graphics = buffer.getGraphics();
	graphics.setColor(color);
	graphics.fillRect(0, 0, width, height);
	return createTexture(buffer);
    }

    public Texture2D createTexture(BufferedImage image) {
	ByteBuffer buffer = BufferUtils.createByteBuffer(image.getWidth() * image.getHeight() * 4);
	ImageToAwt.convert(image, Image.Format.RGBA8, buffer);
	Image jme = new Image(Image.Format.RGBA8, image.getWidth(), image.getHeight(), buffer);
	return new Texture2D(jme);
    }
}
