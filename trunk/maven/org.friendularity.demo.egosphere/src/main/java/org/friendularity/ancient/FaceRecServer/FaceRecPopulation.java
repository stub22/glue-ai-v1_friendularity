package org.friendularity.ancient.FaceRecServer;

import org.cogchar.vision.OpenCVImage;

public class FaceRecPopulation {
	public FaceRecPopulation() {
		m_ptr = createPopulationNative();
	}
	
	protected void finalize() {
		cleanupNative(m_ptr);
	}
	
	public void saveToFile(String filename) {
		saveToFileNative(m_ptr, filename);
	}
	
	public boolean enroll(String name, OpenCVImage[] images) {
		long[] addrs = new long[images.length];
		for ( int i = 0; i < images.length; i++ ) {
			addrs[i] = images[i].raw();
		}
		
		return enrollNative(m_ptr, name, addrs);
	}
	
	private native long createPopulationNative();
	private native void cleanupNative(long pop);
	private native boolean enrollNative(long pop, String name, long[] images);
	private native void saveToFileNative(long pop, String filename);
	//private native void loadFromFileNative(long pop, String filename);
	
	private long m_ptr;
	
	static
    {
	System.loadLibrary("RecognitionServerJNIWrapper");
    }
}
