/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.nwrap.packet;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import java.util.Map;
import java.util.HashMap;
import java.util.logging.Logger;
import org.cogchar.zzz.nwrap.core.EmptyEngine;

/**
 *
 * @author Stu Baurmann
 */
public class PacketBoundary extends EmptyEngine {
	private static Logger	theLogger = Logger.getLogger("com.hansonrobotics.nwrap.packet");	

	// We register all buffers and give them an Integer ID
	public static class RegisteredBuffer {
		public		Integer		id;
		public		ByteBuffer	buffer;
	}
		
	private	static PacketBoundary	thePacketBoundary = new PacketBoundary();
	public	static PacketBoundary		getPacketBoundary() {
		return thePacketBoundary;
	}
	
	private		int myNextBufferID = 0;		
	private static Map<Integer, RegisteredBuffer>		myBufferRegistry;

	public PacketBoundary() {
		myBufferRegistry = new HashMap<Integer, RegisteredBuffer>();
	}

	public ByteBuffer fetchBuffer(Integer id) {
		ByteBuffer result = null;
		RegisteredBuffer	rbuf = myBufferRegistry.get(id);
		result = rbuf.buffer;
		return result;
	}
	
	public synchronized Integer allocateAndMapNativeBuffer(int sizeInBytes) throws Throwable {
		ByteBuffer	bb = allocateNativeMemoryAndByteBuffer(sizeInBytes);
		if (bb == null) {
			throw new Exception("Failed to allocate ByteBuffer of size " + sizeInBytes);
		}
		ByteOrder platformNativeOrder = ByteOrder.nativeOrder();
		bb.order(platformNativeOrder);
		theLogger.fine("Set byteOrder to match platform: " + platformNativeOrder);
		RegisteredBuffer rb = new RegisteredBuffer();
		rb.id = new Integer(myNextBufferID++);
		rb.buffer = bb;
		myBufferRegistry.put(rb.id, rb);
		return rb.id;
	}
	public synchronized void unmapAndDestroyNativeBuffer(Integer id) throws Throwable {
		ByteBuffer bb = fetchBuffer(id);
		if (bb == null) {
			throw new Exception("Can't unmap null buffer found at id: " + id);
		}
		freeNativeMemoryAndByteBuffer(bb);
		myBufferRegistry.remove(id);
	}
	
	/* Intended to be called from the native side to tell Java about a buffer
	 */ 
	public synchronized Integer	registerBuffer(ByteBuffer bb) {
		return null;
	}
	//  Implementatino must  JNI function NewDirectByteBuffer returns a ByteBuffer instance as a jobject
	private static native ByteBuffer	allocateNativeMemoryAndByteBuffer(int byteCount);
	private static native void freeNativeMemoryAndByteBuffer(ByteBuffer bb);
}
