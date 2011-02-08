package rollmadness.sceneparser;

import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import java.util.List;

public interface SceneObject {

    void detach();

    void reattach();

    Spatial getWrappedSpatial();

    Node getSpatialParent();

    List<Geometry> findGeometries();
}
