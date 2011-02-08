package rollmadness.particleengine;

import com.jme3.scene.Node;

public class ParticleGroup implements Particle {
    private final ParticleBehavior BEHAVIOR_CALLBACK = new ParticleBehavior() {

	@Override
	public ParticleBehavior setProperty(Object name, Object value) {
	    for (int i = 0; i < PARTICLES.length; i++) {
		Particle particle = PARTICLES[i];
		particle.getBehavior().setProperty(name, value);
	    }
	    return this;
	}
    };
    private final Particle[] PARTICLES;

    public ParticleGroup(Particle... particles) {
	if(particles == null) {
	    throw new IllegalArgumentException("Particle array cannot be null");
	}
	PARTICLES = particles.clone();
    }

    public Particle setBehavior(ParticleBehavior b) {
	for (int i = 0; i < PARTICLES.length; i++) {
	    Particle particle = PARTICLES[i];
	    particle.setBehavior(b);
	}
	return this;
    }

    public ParticleBehavior getBehavior() {
	return BEHAVIOR_CALLBACK;
    }

    public void update(float tpf) {
	for (int i = 0; i < PARTICLES.length; i++) {
	    Particle particle = PARTICLES[i];
	    particle.update(tpf);
	}
    }

    public void attachTo(Node node) {
	for (int i = 0; i < PARTICLES.length; i++) {
	    Particle particle = PARTICLES[i];
	    particle.attachTo(node);
	}
    }

    public void setEnabled(boolean enabled) {
	for (int i = 0; i < PARTICLES.length; i++) {
	    Particle particle = PARTICLES[i];
	    particle.setEnabled(enabled);
	}
    }

}
