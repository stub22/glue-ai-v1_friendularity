package rollmadness.particleengine;

import com.jme3.asset.AssetManager;
import com.jme3.bounding.BoundingSphere;
import com.jme3.material.Material;
import com.jme3.material.RenderState.FaceCullMode;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Vector3f;
import com.jme3.scene.Geometry;
import com.jme3.scene.Mesh;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.Spatial.CullHint;
import com.jme3.scene.VertexBuffer;
import com.jme3.scene.VertexBuffer.Format;
import com.jme3.scene.VertexBuffer.Usage;
import com.jme3.scene.shape.Quad;
import com.jme3.util.BufferUtils;
import java.nio.ByteBuffer;
import rollmadness.util.MeshTransformer;

public class BasicParticle implements Particle {
    private final Mesh mesh;
    private ParticleBehavior behavior = ParticleBehavior.IDENTITY;
    private final Spatial shape;
    private final ColorRGBA particleColor = new ColorRGBA(1, 1, 1, 1);
    private boolean visible;

    public BasicParticle(Spatial shape) {
	this.shape = shape;
	this.mesh = null;
	shape.setCullHint(CullHint.Always);
    }

    public BasicParticle(AssetManager m, String texture) {
	this(m, texture, new MeshTransformer().translatePositions(new Quad(1, 1), new Vector3f(-0.5f, -0.5f, 0)));
    }

    public BasicParticle(AssetManager assetManager, String texture, Mesh mesh) {
	this.mesh = mesh;
	if(mesh.getBuffer(VertexBuffer.Type.Color) == null) {
	    ByteBuffer cb = BufferUtils.createByteBuffer(4);
	    VertexBuffer cvb = new VertexBuffer(VertexBuffer.Type.Color);
	    cvb.setupData(Usage.Stream, 4, Format.UnsignedByte, cb);
	    cvb.setNormalized(true);

	    mesh.setBuffer(cvb);
	}
	Material material = new Material(assetManager, "Common/MatDefs/Misc/Particle.j3md");
	material.setTexture("m_Texture", assetManager.loadTexture(texture));
	shape = new Geometry("particle", mesh);
	shape.setMaterial(material);
	shape.setModelBound(new BoundingSphere());
	shape.setCullHint(CullHint.Always);
    }

    public void attachTo(Node parent) {
	if(shape.getParent() != parent) {
	    shape.removeFromParent();
	    parent.attachChild(shape);
	    shape.updateGeometricState();
	    shape.updateModelBound();
	}
    }

    public Particle setBehavior(ParticleBehavior b) {
	behavior = b;
	return this;
    }

    public void setEnabled(boolean visible) {
	shape.setCullHint(visible ? CullHint.Never : CullHint.Always);
	if(visible) {
	    behavior.getLife().apply(TimeFunction.START);
	}
	this.visible = visible;
    }

    public void update(float tpf) {
	if(visible) {
	    float time = behavior.getLife().apply(tpf);
	    if(time != TimeFunction.END) {
		if(mesh != null && behavior.getColor() != ColorFunction.IDENTITY) {
		    fillColors(behavior.getColor().apply(particleColor, time, tpf));
		}
		shape.setLocalTranslation(behavior.getTranslation().apply(shape.getLocalTranslation(), time, tpf));
		shape.setLocalScale(behavior.getScale().apply(shape.getLocalScale(), time, tpf));
		shape.setLocalRotation(behavior.getRotation().apply(shape.getLocalRotation(), time, tpf));
		shape.updateGeometricState();
		shape.updateModelBound();
	    } else {
		setEnabled(false);
	    }
	}
    }

    private void fillColors(ColorRGBA c) {
//        VertexBuffer cvb = mesh.getBuffer(VertexBuffer.Type.Color);
//        ByteBuffer colors = (ByteBuffer) cvb.getData();
//	colors.rewind();
//	colors.putInt(c.asIntRGBA());
//	colors.flip();
//	cvb.updateData(colors);
    }

    public void detach() {
	shape.removeFromParent();
    }

    public ParticleBehavior getBehavior() {
	return behavior;
    }
}
