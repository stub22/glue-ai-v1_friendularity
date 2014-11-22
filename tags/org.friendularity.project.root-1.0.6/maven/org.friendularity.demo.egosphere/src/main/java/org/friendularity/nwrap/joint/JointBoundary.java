/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.nwrap.joint;

import org.friendularity.nwrap.blend.InternalBlendingFacade;
import java.nio.ByteBuffer;


/**
 * @author Stu Baurmann
 */
public class JointBoundary {
	
	public static void sendJointAnimationPacket(JointAnimationPacket jap) {
		ByteBuffer	bbuf = jap.getByteBuffer();
		// This int JUST MIGHT be the id of a buffer...
		int	result = executeJointCommand(bbuf);
	}
	private static native int executeJointCommand(ByteBuffer bb);
	
	public static void registerJointFrameTransformer(String role, 
				ITransformJointFrame transformer, 
				JointFramePacket inFramePacket, JointFramePacket outFramePacket, 
				InternalBlendingFacade ibf) {
		// IBF argument is not actually used (yet), instead there's a singleton lookup done
		// inside the native impl.  But passing it as an argument here helps us remember
		// that the IBF system is involved and assumed active.
		
		// This method should not be called before 
		// C++ InternalBlendingFacade.configure() - creates the JavaBlendRule
		// which is called by our native InternalBlendingFacade.startup() method.
		ByteBuffer inFramePacketBuf = inFramePacket.getByteBuffer();
		ByteBuffer outFramePacketBuf = outFramePacket.getByteBuffer();
		int result = nativeRegisterJointFrameTransformer(role, transformer, inFramePacketBuf, 
					outFramePacketBuf);
	}
	
	private static native int nativeRegisterJointFrameTransformer(String role, 
				Object transformer, ByteBuffer inFrameBuf, ByteBuffer outFrameBuf);
}
