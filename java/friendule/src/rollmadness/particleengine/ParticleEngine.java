package rollmadness.particleengine;

import com.jme3.renderer.RenderManager;
import com.jme3.renderer.ViewPort;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.control.AbstractControl;
import com.jme3.scene.control.Control;
import java.util.LinkedHashMap;
import java.util.Map;

public class ParticleEngine extends AbstractControl {
    private final Map<String, Particle> registry = new LinkedHashMap<String, Particle>();
    private Node hook;

    public ParticleEngine(Node parent) {
	setEnabled(true);
	hook = parent;
	hook.addControl(this);
    }

    public void addParticle(String key, Particle p) {
	registry.put(key, p);
	if(hook != null) {
	    p.attachTo(hook);
	}
    }

    public Particle getParticle(String key) {
	return registry.get(key);
    }

    @Override
    protected void controlUpdate(float tpf) {
	for (Particle particle : registry.values()) {
	    particle.update(tpf);
	}
    }

    @Override
    protected void controlRender(RenderManager rm, ViewPort vp) {
    }

    public Control cloneForSpatial(Spatial spatial) {
	return this;
    }

}
