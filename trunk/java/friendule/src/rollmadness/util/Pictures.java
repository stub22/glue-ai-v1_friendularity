package rollmadness.util;

import com.jme3.asset.AssetManager;
import com.jme3.asset.TextureKey;
import com.jme3.texture.Texture2D;
import com.jme3.ui.Picture;

public class Pictures {

    public Picture newPicture(AssetManager am, TextureKey textureKey) {
	Texture2D texture = (Texture2D) am.loadTexture(textureKey);
	Picture picture = new Picture("");
	picture.setTexture(am, texture, true);
	picture.setWidth(texture.getImage().getWidth());
	picture.setHeight(texture.getImage().getHeight());
	return picture;
    }

    public Picture newPicture(AssetManager am, String tex) {
	Texture2D texture = (Texture2D) am.loadTexture(tex);
	Picture picture = new Picture("");
	picture.setTexture(am, texture, true);
	picture.setWidth(texture.getImage().getWidth());
	picture.setHeight(texture.getImage().getHeight());
	return picture;
    }
}
