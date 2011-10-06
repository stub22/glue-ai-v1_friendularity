/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package		org.friendularity.nwrap.joint;


import		java.util.HashMap;
import		java.util.Map;
import		javolution.io.Struct;
import		java.util.logging.Logger;
import org.cogchar.animoid.protocol.Frame;
import org.cogchar.animoid.protocol.JointPosition;
import org.cogchar.animoid.protocol.JointPositionAROM;
import org.cogchar.animoid.protocol.JointStateCoordinateType;

/**
 * @author Stu Baurmann
 */

public class JointFrameStruct extends Struct {
	private static Logger	theLogger = Logger.getLogger("com.hansonrobotics.nwrap.joint.JointFrameStruct");	
	public static int		MAX_JOINTS =		32;

	public	final	JointInstructionStruct		instructions[] = array(new JointInstructionStruct[MAX_JOINTS]);
	
	protected void initAfterMapped() {
		theLogger.finest("JFS initAfterMapped()");
		for (int i=0; i < MAX_JOINTS; i++) {
			instructions[i].initAfterMapped();
		}
	}
	public void clearAllInstructions() {
		for (int i=0; i < MAX_JOINTS; i++) {
			instructions[i].clear();
		}
	}
	private JointInstructionStruct findFirstEmptyInstruction() {
		JointInstructionStruct result = null;
		for (int i=0; i < MAX_JOINTS; i++) {
			JointInstructionStruct ji = instructions[i];
			if(ji.jointCommand.get() == JointCommand.EMPTY) {
				result = ji;
				break;
			}
		}
		return result;
	}
	public JointInstructionStruct findInstructionForLogicalJoint(int logJoint) {
		JointInstructionStruct result = null;
		for (int i=0; i < MAX_JOINTS; i++) {
			JointInstructionStruct ji = instructions[i];
			int jid = (int) ji.logicalJointID.get();
			if (jid == logJoint) {
				result = ji;
				break;
			}
		}
		return result;
	}
	public JointInstructionStruct findOrAddInstructionForLogicalJoint(int logJointID) {
		JointInstructionStruct result = findInstructionForLogicalJoint(logJointID);
		if (result == null) {
			result = findFirstEmptyInstruction();
			result.setLogicalJointID(logJointID);
		}
		return result;
	}	
	public void addInstruction(int logJoint, JointCommand cmd, double jointValue) {
		// Make sure we're already mapped, and if not, throw?
		JointInstructionStruct ji = findFirstEmptyInstruction();
		if (ji == null) {
			throw new RuntimeException("Can't find empty instruction slot for joint# " + logJoint + " command " + cmd);
		}
		ji.setLogicalJointID(logJoint);
		ji.setJointCommand(cmd);
		ji.setTargetJointValue(jointValue);
	}	
	public void setInstructionForLogicalJoint(int logJointID, JointCommand cmd, 
				double jointValue) {
		JointInstructionStruct jis = findOrAddInstructionForLogicalJoint(logJointID);
		jis.setJointCommand(cmd);
		jis.setTargetJointValue(jointValue);		
	}
	public void addAbsoluteImmediateMove(int logJoint, double jointValue) {
		addInstruction(logJoint, JointCommand.MOVE_ABSOLUTE_IMMEDIATE, jointValue);
	}
	public void setAbsoluteImmediateMoveForLogicalJoint(int logJoint, double jointValue) {
		setInstructionForLogicalJoint(logJoint, JointCommand.MOVE_ABSOLUTE_IMMEDIATE, jointValue);
	}
	public Map<Integer,JointInstructionStruct> getNonemptyInstructionsByLogicalJoint() {
		HashMap<Integer,JointInstructionStruct> result = new HashMap<Integer,JointInstructionStruct>();
		for (int i=0; i < MAX_JOINTS; i++) {
			JointInstructionStruct ji = instructions[i];
			if (ji.jointCommand.get() != JointCommand.EMPTY) {
				Integer jointID = (int) ji.logicalJointID.get();
				result.put(jointID, ji);
			}
		}
		return result;
	}
	public void snapshotMatchingValuesIntoMap(JointCommand cmdToMatch, Map<Integer,Double> targetMap) {
		for (int i=0; i < MAX_JOINTS; i++) {
			JointInstructionStruct ji = instructions[i];			
			if (ji.jointCommand.get() == cmdToMatch) {
				Integer jointID = (int) ji.logicalJointID.get();
				targetMap.put(jointID, ji.targetJointValue.get());
			}
		}
	}
	public void overwriteWithAbsRomData(Frame<JointPositionAROM> absRomFrame) {
		clearAllInstructions();
		Frame<JointPosition> lopsidedOutputFrame = absRomFrame.copyAndConvert(JointStateCoordinateType.FLOAT_ABS_LOPSIDED_PIECEWISE_LINEAR);
		
		for (JointPosition jp: lopsidedOutputFrame.getAllPositions()) {
			int logJoint = jp.getJoint().oldLogicalJointNumber;
			Double lopsidedPos = jp.getCoordinateFloat(JointStateCoordinateType.FLOAT_ABS_LOPSIDED_PIECEWISE_LINEAR);
			addAbsoluteImmediateMove(logJoint, lopsidedPos);
		}
	}	
}
