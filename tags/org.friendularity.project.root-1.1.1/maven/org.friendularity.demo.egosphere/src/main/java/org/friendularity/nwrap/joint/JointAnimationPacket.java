/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package		org.friendularity.nwrap.joint;

import		java.util.List;
import		java.util.ArrayList;

import		org.friendularity.nwrap.packet.StructPacket;
import java.util.Map;
import		java.util.logging.Logger;

/**
 * @author Stu Baurmann
 */

public class JointAnimationPacket extends StructPacket {
	private static Logger	theLogger = Logger.getLogger(JointAnimationPacket.class.getName());

	public	enum JointAnimationCommand {
		EXECUTE_NOW,
		ADD_NAMED_TO_LIBRARY
	}
	private	static	List<JointAnimationCommand>		theJointAnimationCommandList;
	public static	List<JointAnimationCommand>		getJointAnimationCommandList() {
		if (theJointAnimationCommandList == null) {
			theJointAnimationCommandList = new ArrayList<JointAnimationCommand>();
			// Order here must be the same as order in the Enum decl, and 
			// as the order of integer constants on the C++ side.
			theJointAnimationCommandList.add(JointAnimationCommand.EXECUTE_NOW);
			theJointAnimationCommandList.add(JointAnimationCommand.ADD_NAMED_TO_LIBRARY);				
		}
		return theJointAnimationCommandList;
	}			
	public final	Enum32					animationCommand		= null; // new Enum32(getJointAnimationCommandList());		
	public final	Unsigned32				frameCount				= new Unsigned32();	
	// Pad the structure so that frames array begins on a 32 byte boundary.
	public final	Unsigned32				dummy01					= new Unsigned32();	
	public final	Unsigned32				dummy02					= new Unsigned32();	
	public final	Unsigned32				dummy03					= new Unsigned32();	
	public final	Unsigned32				dummy04					= new Unsigned32();	
	public final	Unsigned32				dummy05					= new Unsigned32();	
	public final	Unsigned32				dummy06					= new Unsigned32();	
	
	public final	UTF8String				animationName			= new UTF8String(224);
	public			JointFrameStruct		frames[];
	
	public JointAnimationPacket(int frameCount) {
		frames = array(new JointFrameStruct[frameCount]);
	}
	
	protected void initAfterMapped() {
		theLogger.fine("JAP initAfterMapped()");
		super.initAfterMapped();
		frameCount.set(frames.length);
		for (int i=0; i < frames.length; i++) {
			frames[i].initAfterMapped();
		}
	}
	public List<Map<Integer,JointInstructionStruct>> getLogicalFrameMaps() {
		List<Map<Integer,JointInstructionStruct>> frameMapList = new ArrayList<Map<Integer,JointInstructionStruct>>();
		for (int i=0; i < frames.length; i++) {
			Map<Integer,JointInstructionStruct> frameMap = frames[i].getNonemptyInstructionsByLogicalJoint();
			frameMapList.add(frameMap);
		}
		return frameMapList;
	}
}
