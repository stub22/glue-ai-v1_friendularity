package rollmadness.util;

import com.jme3.asset.AssetManager;
import com.jme3.material.MatParam;
import com.jme3.material.Material;
import com.jme3.renderer.Camera;
import com.jme3.renderer.RenderManager;
import com.jme3.renderer.ViewPort;
import com.jme3.renderer.queue.RenderQueue.Bucket;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.control.AbstractControl;
import com.jme3.scene.control.Control;
import com.jme3.shader.VarType;
import com.jme3.texture.Texture;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

public class DefaultSkyBox extends AbstractControl {
    private Node parent;
    private Spatial skybox;
    private final Camera CAMERA;
    private final AssetManager ASSET_MANAGER;

    public DefaultSkyBox(Camera camera, AssetManager am) {
	CAMERA = camera;
	ASSET_MANAGER = am;
    }

    public void set(Spatial skybox) {
	if(this.skybox != null && parent != null) {
	    detach();
	    skybox.setQueueBucket(Bucket.Sky);
	    attachTo(parent);
	} else {
	    this.skybox = skybox;
	    if(parent != null) {
		attachTo(parent);
	    }
	}
	setEnabled(skybox != null);
	if(skybox != null) {
	    applyMaterial(skybox);
	}
    }

    public void attachTo(Node rootNode) {
	if(parent != null) {
	    detach();
	}
	this.parent = rootNode;
	this.parent.addControl(this);
	if(skybox != null) {
	    parent.attachChild(skybox);
	}
	setEnabled(skybox != null);
    }

    public void detach() {
	if(skybox != null) {
	    skybox.removeFromParent();
	    parent.removeControl(this);
	}
    }

    @Override
    protected void controlUpdate(float tpf) {
	skybox.setLocalTranslation(CAMERA.getLocation());
    }

    @Override
    protected void controlRender(RenderManager rm, ViewPort vp) {
    }

    public Control cloneForSpatial(Spatial spatial) {
	return this;
    }

    private void applyMaterial(Spatial skybox) {
	
	for(Spatial s : new SceneIterable(skybox)) {
	    if(s instanceof Geometry) {
		Geometry g = (Geometry)s;
		Material m = g.getMaterial();
		Collection<MatParam> params = m.getParams();
		for (MatParam p : params) {
		    String name = p.getName();
		    if(p.getVarType() == VarType.Texture2D && (name.equals("m_ColorMap") || name.equals("m_DiffuseMap"))) {
			setMaterial(g, m.getTextureParam(name).getTextureValue());
			return;
		    }
		}
	    }
	}
    }

    private void setMaterial(Geometry geom, Texture textureValue) {
	Logger.getLogger(getClass().getName()).log(Level.INFO, "Set simple textured material...");
	Material mat = new Material(ASSET_MANAGER, "Common/MatDefs/Misc/SimpleTextured.j3md");
	mat.setTexture("m_ColorMap", textureValue);
	geom.setMaterial(mat);
    }

}
