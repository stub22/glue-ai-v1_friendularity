package rollmadness.particleengine;

import com.jme3.scene.Node;

/**
 * A ring of particles.
 * @author pgi
 */
public class ParticleRing implements Particle {
    private Particle[] particles;
    private int nextparticle = 0;
    private boolean enabled = true;

    public ParticleRing(Particle... particles) {
	if(particles == null || particles.length == 0) {
	    throw new IllegalArgumentException("ParticleRing cannot be initialized with a null or empty array of particles.");
	}
	this.particles = particles;
    }

    public Particle next() {
	Particle n = particles[nextparticle];
	nextparticle++;
	if(nextparticle == particles.length) {
	    nextparticle = 0;
	}
	return n;
    }

    public Particle setBehavior(ParticleBehavior b) {
	return this;
    }

    public ParticleBehavior getBehavior() {
	return ParticleBehavior.IDENTITY;
    }

    public void update(float tpf) {
	if(enabled) {
	    for (int i = 0; i < particles.length; i++) {
		Particle particle = particles[i];
		particle.update(tpf);
	    }
	}
    }

    public void attachTo(Node node) {
	for (int i = 0; i < particles.length; i++) {
	    Particle particle = particles[i];
	    particle.attachTo(node);
	}
    }

    public void setEnabled(boolean enabled) {
	this.enabled = enabled;
    }

}
