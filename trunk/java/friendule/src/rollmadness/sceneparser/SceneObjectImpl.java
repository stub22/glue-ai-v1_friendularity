package rollmadness.sceneparser;

import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import java.util.LinkedList;
import java.util.List;

class SceneObjectImpl implements SceneObject {

    private final Spatial spatial;
    private final Node parent;

    SceneObjectImpl(Spatial spatial) {
	this.spatial = spatial;
	this.parent = spatial.getParent();
    }

    public void detach() {
	if (parent != null) {
	    spatial.removeFromParent();
	}
    }

    public void reattach() {
	if (spatial.getParent() == null) {
	    parent.attachChild(spatial);
	}
    }

    public Spatial getWrappedSpatial() {
	return spatial;
    }

    public Node getSpatialParent() {
	return parent;
    }

    public List<Geometry> findGeometries() {
	List<Geometry> g = new LinkedList<Geometry>();
	LinkedList<Spatial> list = new LinkedList<Spatial>();
	list.add(spatial);
	while (!list.isEmpty()) {
	    Spatial pop = list.pop();
	    if (pop instanceof Geometry) {
		g.add((Geometry) pop);
	    } else if (pop instanceof Node) {
		list.addAll(((Node) pop).getChildren());
	    }
	}
	return g;
    }
}
