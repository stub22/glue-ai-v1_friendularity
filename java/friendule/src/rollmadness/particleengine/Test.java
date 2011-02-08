package rollmadness.particleengine;

import com.jme3.app.SimpleApplication;
import com.jme3.input.controls.ActionListener;
import com.jme3.input.controls.MouseButtonTrigger;
import rollmadness.particleengine.behaviors.PopStarBehavior;

public class Test extends SimpleApplication {

    public static void main(String[] args) {
	new Test().start();
    }
//    private Particle particle;

    @Override
    public void simpleInitApp() {
	float speed = 8;
	ParticleGroup particle = new ParticleGroup(
		new BasicParticle(assetManager, "rollmadness/textures/star.png").setBehavior(new PopStarBehavior(speed)),
		new BasicParticle(assetManager, "rollmadness/textures/star.png").setBehavior(new PopStarBehavior(speed)),
		new BasicParticle(assetManager, "rollmadness/textures/star.png").setBehavior(new PopStarBehavior(speed)),
		new BasicParticle(assetManager, "rollmadness/textures/star.png").setBehavior(new PopStarBehavior(speed)),
		new BasicParticle(assetManager, "rollmadness/textures/star.png").setBehavior(new PopStarBehavior(speed)));
//	BasicParticle particle = new BasicParticle(assetManager, "rollmadness/textures/star.png");
//	particle.setBehavior(new PopStarBehavior());
	final ParticleEngine pe = new ParticleEngine(rootNode);
	pe.addParticle("bullet", particle);
	inputManager.setCursorVisible(true);
	inputManager.addMapping("shoot", new MouseButtonTrigger(0));
	inputManager.addListener(new ActionListener() {

	    public void onAction(String name, boolean value, float tpf) {
		if(!value) {
		    Particle p = pe.getParticle("bullet");
		    ParticleBehavior b = p.getBehavior();
		    b.setProperty(PopStarBehavior.KEY_ORIGIN, cam.getLocation().add(cam.getDirection().mult(10)));
		    p.setEnabled(true);
		}
	    }
	}, "shoot");
    }
}
