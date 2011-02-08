package rollmadness.logic.triggers;

import com.jme3.audio.AudioNode;
import com.jme3.audio.AudioRenderer;
import com.jme3.bounding.BoundingVolume;
import com.jme3.math.Vector3f;
import com.jme3.scene.Spatial;
import java.util.ArrayList;
import java.util.List;
import jme3clogic.Condition;
import jme3clogic.Reaction;
import jme3clogic.Trigger;
import rollmadness.sceneparser.SceneObject;
import rollmadness.sceneparser.SceneObjectGenerator;

/**
 * This trigger controls the warp volumes: the player enters a volume and
 * exits somewhere else in the track
 * @author pgi
 */
public class WarpTrigger extends Trigger {

    public WarpTrigger(final Spatial track, final Spatial player, final AudioNode warpsound, final AudioRenderer audioRenderer) {
	SceneObjectGenerator gen = new SceneObjectGenerator();
	gen.setIgnoreCase(true).setIgnoredCharacters("#").setMatchIfContains(true);
	List<SceneObject> warpins = gen.generate(track, "volume_warpin").getSceneObjects("volume_warpin");
	List<SceneObject> warpout = gen.generate(track, "volume_warpout").getSceneObjects("volume_warpout");
	final ArrayList<Portal> portals = new ArrayList<Portal>();
	for (SceneObject in : warpins) {
	    SceneObject out = getOut(in, warpout);
	    if(out != null) {
		System.out.println("Warp from " + in.getWrappedSpatial().getName() + " to " + out.getWrappedSpatial().getName());
		portals.add(new Portal(in.getWrappedSpatial().getWorldBound(), out.getWrappedSpatial().getWorldBound()));
	    }
	}
	final Vector3f outLocation = new Vector3f();
	condition = new Condition() {

	    @Override
	    public boolean holds(float timePerFrame) {
		for(int i = 0; i < portals.size(); i++) {
		    Portal portal = portals.get(i);
		    if(portal.in.contains(player.getLocalTranslation())) {
			outLocation.set(portal.out.getCenter());
			return true;
		    }
		}
		return false;
	    }
	};
	reaction = new Reaction() {

	    @Override
	    public void act(float timePerFrame) {
		player.setLocalTranslation(outLocation.x, outLocation.y, outLocation.z);
		audioRenderer.playSource(warpsound);
	    }
	};
    }

    private boolean areBound(Spatial warpin, Spatial warpout) {
	String indexa = getIndex(warpin.getName());
	String indexb = getIndex(warpout.getName());
	return indexa.equals(indexb);
    }

    private String getIndex(String name) {
	StringBuilder buffer = new StringBuilder();
	for(int i = 0; i < name.length(); i++) {
	    char c = name.charAt(i);
	    if(Character.isDigit(c)) {
		buffer.append(c);
	    }
	}
	return buffer.toString();
    }

    private SceneObject getOut(SceneObject in, List<SceneObject> warpout) {
	for (SceneObject e : warpout) {
	    if(areBound(in.getWrappedSpatial(), e.getWrappedSpatial())) {
		return e;
	    }
	}
	return null;
    }

    private static class Portal {
	BoundingVolume in, out;

	Portal(BoundingVolume in, BoundingVolume out) {
	    this.in = in;
	    this.out = out;
	}
    }

}
