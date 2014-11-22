/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.nwrap.packet;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import java.util.logging.Logger;
import	javolution.io.Struct;


/**
 *
 * @author Stu Baurmann
 */
// We use 32 or 64 bit values for all scalars.
// We use 64 bit pointers.

/* All nested structures, including this "header" data, must be sized in multiples of 32 bytes,
 * to work with WIN_32 arch.  We may need to go to even higher multiples.
 */

public class StructPacket extends Struct {
	private static Logger	theLogger = Logger.getLogger("com.hansonrobotics.nwrap.packet.StructPacket");
	
	
	// Same as the bufferID returned from allocateAndMapNativeBuffer
	public			final		Unsigned32			bufferID			= new Unsigned32();	
	// Total size including this header and all app-specific data.
	public			final		Unsigned32			packetSize			= new Unsigned32();
	// Should be 32 or 64
	public			final		Unsigned32			platformWidthBits	= new Unsigned32();
	public			final		Enum32				platformKind		= null; // new Enum32(Platform.getPlatformKindEnumList());
	public			final		Enum32				allocationStyle		= null; // new Enum32(Allocator.getAllocatorStyleEnumList());
	// Pad initial portion of structure out to 32 bytes = 256 bits
	public			final		Unsigned32			dummyPadding01		= new Unsigned32();	
	public			final		Unsigned32			dummyPadding02		= new Unsigned32();	
	public			final		Unsigned32			dummyPadding03		= new Unsigned32();		
	// Written into the struct for debugging/sanity-check on native side.
	public			final		UTF8String			javaClassName		= new UTF8String(128);

     public ByteOrder byteOrder() {
         return ByteOrder.nativeOrder();
    }	
	
	protected void initAfterMapped() {

		// This is dangerous - creates an (unmapped) buffer if not existing.
		// ByteBuffer	byteBuf = this.getByteBuffer();
		
		packetSize.set(this.size());		
		String jcn = this.getClass().getName();
		javaClassName.set(jcn);
		platformKind.set(Platform.Kind.MSWIN_32);
		platformWidthBits.set(32);
		allocationStyle.set(Allocator.Style.FREED_BY_JAVA);
	}
	/*
	 * Needs to happen before any other methods are called or values are set!!!
	 */
	public void assignRegisteredBuffer(Integer bufID, ByteBuffer byteBuf) {
		this.setByteBuffer(byteBuf, 0);
		bufferID.set(bufID);
		initAfterMapped();
	}
	public void mapAndAllocate() throws Throwable {
		PacketBoundary pb = PacketBoundary.getPacketBoundary();
		int size = this.size();
		Integer bufID = pb.allocateAndMapNativeBuffer(size);
		ByteBuffer bbuf = pb.fetchBuffer(bufID);
		assignRegisteredBuffer(bufID, bbuf);
	}
	public void unmapAndDestroy() throws Throwable {
		PacketBoundary pb = PacketBoundary.getPacketBoundary();
		long	bufID = bufferID.get();
		Integer biid = new Integer((int) bufID);
		pb.unmapAndDestroyNativeBuffer(biid);
	}
	/*
	public void copyBytesFromPacket(StructPacket otherPacket) {
		class otherClass = otherPacket.getClass()
		int otherSize = otherPacket.size();
	}
	 */
}
