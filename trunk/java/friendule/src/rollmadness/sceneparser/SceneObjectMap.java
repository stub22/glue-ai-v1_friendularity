package rollmadness.sceneparser;

import java.util.List;

public interface SceneObjectMap {

    List<SceneObject> getSceneObjects(String id);

    SceneObject getFirstOf(String id);

    void detachAll(String... ids);

    void reattachAll(String...ids);
}
