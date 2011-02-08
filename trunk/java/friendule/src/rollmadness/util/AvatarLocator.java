package rollmadness.util;

import com.jme3.app.SimpleApplication;
import com.jme3.input.KeyInput;
import com.jme3.input.controls.ActionListener;
import com.jme3.input.controls.KeyTrigger;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Transform;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import java.awt.Color;
import java.util.logging.Level;
import java.util.logging.Logger;
import jme3dae.ColladaDocumentFactory;
import jme3dae.ColladaDocumentFactory.FXSettingsGenerator;
import jme3dae.ColladaLoader;
import jme3dae.FXEnhancerInfo;
import jme3dae.FXEnhancerInfo.IgnoreMeasuringUnit;

public class AvatarLocator extends SimpleApplication {

    public static void main(String[] args) {
	new AvatarLocator().start();
    }
    Spatial avatar;
    float dt = 0.05f;
    Quaternion dqx;

    @Override
    public void simpleInitApp() {
	FXEnhancerInfo info = new FXSettingsGenerator().setIgnoreMeasuringUnit(IgnoreMeasuringUnit.ON).get();
	ColladaDocumentFactory.setFXEnhance(info);
	viewPort.setBackgroundColor(ColorRGBA.White);
	assetManager.registerLoader(ColladaLoader.class.getName(), "dae");
	dqx = new Quaternion().fromAngleAxis(FastMath.PI / 50, Vector3f.UNIT_X.clone());
//	assetManager.registerLocator(getClass().getResource("/rollmadness/models/").toString(), UrlLocator.class.getName());
	avatar = assetManager.loadModel("/rollmadness/models/playercapsule.dae");
	avatar.setLocalRotation(cam.getRotation());
	avatar.setLocalTranslation(cam.getLocation());
	inputManager.removeListener(flyCam);
	rootNode.attachChild(avatar);
	createListener("moveup", KeyInput.KEY_UP);
	createListener("movedown", KeyInput.KEY_DOWN);
	createListener("moveleft", KeyInput.KEY_LEFT);
	createListener("moveright", KeyInput.KEY_RIGHT);
	createListener("moveon", KeyInput.KEY_NUMPAD8);
	createListener("moveback", KeyInput.KEY_NUMPAD2);
	createListener("rotateup", KeyInput.KEY_PGUP);
	createListener("rotatedown", KeyInput.KEY_PGDN);
	new Lights().createDirectionalLight(rootNode, new Vector3f(-1, -1, -1), Color.WHITE);
	Node n = (Node) avatar;
	for (Spatial spatial : n.getChildren()) {
	    System.out.println(spatial);
	}
    }

    public void moveon() {
	avatar.setLocalTranslation(avatar.getLocalTranslation().add(0, 0, dt));
    }

    public void moveback() {
	avatar.setLocalTranslation(avatar.getLocalTranslation().add(0, 0, -dt));
    }

    public void moveup() {
	avatar.setLocalTranslation(avatar.getLocalTranslation().add(0, dt, 0));
    }

    public void movedown() {
	avatar.setLocalTranslation(avatar.getLocalTranslation().add(0, -dt, 0));
    }

    public void moveleft() {
	avatar.setLocalTranslation(avatar.getLocalTranslation().add(dt, 0, 0));
    }

    public void moveright() {
	avatar.setLocalTranslation(avatar.getLocalTranslation().add(-dt, 0, 0));
    }

    public void rotateup() {
	avatar.setLocalRotation(avatar.getLocalRotation().multLocal(dqx));
    }

    public void rotatedown() {
	avatar.setLocalRotation(avatar.getLocalRotation().multLocal(dqx.inverse()));
    }

    private void createListener(String key, int k) {
	inputManager.addMapping(key, new KeyTrigger(k));
	inputManager.addListener(new RefListener(key), key);
    }

    class RefListener implements ActionListener {

	private String m;

	RefListener(String s) {
	    m = s;
	}

	public void onAction(String name, boolean value, float tpf) {
	    if (!value) {
		try {
		    AvatarLocator.class.getMethod(m).invoke(AvatarLocator.this);
		    Transform t = avatar.getLocalTransform();
		    System.out.println(t.getTranslation());
		    float[] rot = new float[3];
		    t.getRotation().toAngles(rot);
		    System.out.println("Rx:" + (FastMath.RAD_TO_DEG * rot[0])
			    + " Ry: " + (FastMath.RAD_TO_DEG * rot[1]) +
			    " Rz: " + (FastMath.RAD_TO_DEG * rot[2]));
		} catch (Exception ex) {
		    Logger.getLogger(AvatarLocator.class.getName()).log(Level.SEVERE, null, ex);
		}
	    }
	}
    }
}
