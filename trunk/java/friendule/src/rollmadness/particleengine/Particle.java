package rollmadness.particleengine;

import com.jme3.scene.Node;

public interface Particle {

    Particle setBehavior(ParticleBehavior b);

    ParticleBehavior getBehavior();

    void update(float tpf);

    void attachTo(Node node);

    void setEnabled(boolean enabled);
}
