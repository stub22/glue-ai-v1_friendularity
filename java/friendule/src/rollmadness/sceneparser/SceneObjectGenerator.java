package rollmadness.sceneparser;

import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class SceneObjectGenerator {

    private boolean ignoreCase = false;
    private boolean matchIfContains = false;
    private boolean matchIfStartsWith = false;
    private String ignoredChars;

    public SceneObjectGenerator setIgnoredCharacters(String ignoredCharacters) {
	this.ignoredChars = ignoredCharacters;
	return this;
    }

    public SceneObjectGenerator setIgnoreCase(boolean activate) {
	this.ignoreCase = activate;
	return this;
    }

    public SceneObjectGenerator setMatchIfContains(boolean activate) {
	this.matchIfContains = activate;
	if (activate) {
	    matchIfStartsWith = false;
	}
	return this;
    }

    public SceneObjectGenerator setMatchIfStartsWith(boolean activate) {
	this.matchIfStartsWith = activate;
	if (activate) {
	    matchIfContains = false;
	}
	return this;
    }

    public SceneObjectMap generate(Spatial node, String... names) {
	final Map<String, List<SceneObject>> map = new HashMap<String, List<SceneObject>>();
	final LinkedList<Spatial> queue = new LinkedList<Spatial>();
	queue.add(node);
	while (!queue.isEmpty()) {
	    Spatial s = queue.pop();
	    for (int i = 0; i < names.length; i++) {
		String name = names[i];
		if (spatialNameMatches(s, name)) {
		    push(s, map, name);
		} else {
		}
	    }
	    if (s instanceof Node) {
		queue.addAll(((Node) s).getChildren());
	    }
	}
	return new SceneObjectMapImpl(map);
    }

    private boolean spatialNameMatches(Spatial s, String name) {
	if (s.getName() == null || s.getName().trim().isEmpty()) {
	    return false;
	}
	String spatialName = s.getName();
	if (ignoreCase) {
	    spatialName = spatialName.toLowerCase();
	    name = name.toLowerCase();
	}
	if (ignoredChars != null) {
	    for (int i = 0; i < ignoredChars.length(); i++) {
		String match = ignoredChars.substring(i, i + 1);
		spatialName = spatialName.replace(match, "");
		spatialName = spatialName.replace((CharSequence) match, (CharSequence) "");
	    }
	}
	if (matchIfContains) {
	    return spatialName.contains(name);
	} else if (matchIfStartsWith) {
	    return spatialName.startsWith(name);
	} else {
	    return spatialName.equals(name);
	}

    }

    private void push(Spatial s, Map<String, List<SceneObject>> map, String name) {
	List<SceneObject> list = map.get(name);
	if (list == null) {
	    map.put(name, list = new LinkedList<SceneObject>());
	}
	list.add(new SceneObjectImpl(s));
    }
}
