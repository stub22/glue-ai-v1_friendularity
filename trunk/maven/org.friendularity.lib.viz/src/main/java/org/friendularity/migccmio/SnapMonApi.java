package org.friendularity.migccmio;

import com.jme3.scene.Node;
import org.cogchar.bind.midi.in.CCParamRouter;
import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.friendularity.visual.texture.JVisionTextureMapper;

/**
 * Created by Owner on 2/16/2017.
 */
public interface SnapMonApi {
	public void setup_onRendThrd(RenderRegistryClient rrc, Node parentNode);
	public void setJVisionTextureMapper(JVisionTextureMapper jvtm);
	public void update_onRendThrd(float tpf);
	public void attachMidiCCs(CCParamRouter ccpr); // Matches method in TrialContent
}
