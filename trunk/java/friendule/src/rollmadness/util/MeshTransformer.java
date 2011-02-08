package rollmadness.util;

import com.jme3.math.Vector3f;
import com.jme3.scene.Mesh;
import com.jme3.scene.VertexBuffer;
import com.jme3.scene.VertexBuffer.Type;
import java.nio.Buffer;
import java.nio.FloatBuffer;

public class MeshTransformer {

    public Mesh translatePositions(Mesh mesh, Vector3f trans) {
	VertexBuffer buffer = mesh.getBuffer(Type.Position);
	Buffer data = buffer.getData();
	if(data instanceof FloatBuffer && buffer.getNumComponents() == 3) {
	    FloatBuffer b = (FloatBuffer) data;
	    for(int i = 0; i < b.capacity(); i+=3) {
		b.put(i, b.get(i) + trans.x);
		b.put(i+1, b.get(i+1) + trans.y);
		b.put(i+2, b.get(i+2) + trans.z);
	    }
	    buffer.updateData(data);
	}
	return mesh;
    }
}
