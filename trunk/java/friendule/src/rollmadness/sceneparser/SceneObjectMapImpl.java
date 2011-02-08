package rollmadness.sceneparser;

import java.util.Collections;
import java.util.List;
import java.util.Map;

class SceneObjectMapImpl implements SceneObjectMap {

    private final Map<String, List<SceneObject>> map;

    SceneObjectMapImpl(Map<String, List<SceneObject>> map) {
	this.map = map;
    }

    public List<SceneObject> getSceneObjects(String id) {
	if (!map.containsKey(id)) {
	    System.out.println("NO ELEMENTS FOR KEY: " + id);
	}
	return map.containsKey(id) ? map.get(id) : Collections.<SceneObject>emptyList();
    }

    public SceneObject getFirstOf(String id) {
	SceneObject object = null;
	if (map.containsKey(id)) {
	    List<SceneObject> list = map.get(id);
	    if (!list.isEmpty()) {
		object = list.get(0);
	    }
	}
	return object;
    }

    public void detachAll(String... ids) {
	for (int i = 0; i < ids.length; i++) {
	    for (SceneObject sceneObject : getSceneObjects(ids[i])) {
		sceneObject.detach();
	    }
	}
    }

    public void reattachAll(String... ids) {
	for (int i = 0; i < ids.length; i++) {
	    for (SceneObject sceneObject : getSceneObjects(ids[i])) {
		sceneObject.reattach();
	    }
	}
    }
}
