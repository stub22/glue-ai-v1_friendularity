package org.friendularity.ancient.FaceRecServer;


/* These FaceRecServer classes are so named in order to match the Native functions in 
 * the RecognitionServerJNIWrapper.dll
 */

import java.util.Collection;

import org.cogchar.sight.api.obs.OpenCVImage;
import org.freckler.facerec.impl.nwrap.FaceProfile;
import org.freckler.facerec.impl.nwrap.FaceProfileManager;
import org.freckler.facerec.impl.nwrap.FaceRecPopulationManager;


public class FaceRecServer implements FaceRecPopulationManager, FaceProfileManager {
	public FaceRecServer(String config) {
		m_config = config;
		m_defaultPopulation = -1;
	}
	
	public void start() {
		startServerNative(m_config);
	}
	
	public void shutdown() {
		shutdownServerNative();
	}
	
	public void addObserver(ProcessingObserver obs) {
		addObserverNative(obs);
	}
	
	public void removeObserver(ProcessingObserver obs) {
		removeObserverNative(obs);
	}
	
	private native void startServerNative(String config);
	private native void shutdownServerNative();
	private native void addObserverNative(ProcessingObserver obs);
	private native void removeObserverNative(ProcessingObserver obs);
	
	// Actual queries
	public long createPopulation() {
		return CreatePopulationNative();
	}
	public void destroyPopulation(long pop_id) {
		DestroyPopulationNative(pop_id);
	}

	public String matchPersonInDefaultPop(OpenCVImage image) {
		return matchPerson(image, getDefaultPopulationID());
	}
	public String matchPerson(OpenCVImage image, long population) {
		return MatchPersonNative(image.raw(), population);
	}

	public String matchOrAddPersonInDefaultPop(OpenCVImage image) {
		return matchOrAddPerson(image, getDefaultPopulationID());
	}
	public String matchOrAddPerson(OpenCVImage image, long pop_id) {
		return MatchOrAddPersonNative(image.raw(), pop_id);
	}

	public boolean addPersonToDefaultPop(OpenCVImage image) {
		return addPerson(image, getDefaultPopulationID());
	}
	public boolean addPerson(OpenCVImage image, long pop_id) {
		return AddPersonNative(image.raw(), pop_id);
	}
	
	public boolean addNamedPersonToDefaultPop(OpenCVImage image, String name) {
		return addNamedPerson(image, name, getDefaultPopulationID());
	}
	public boolean addNamedPerson(OpenCVImage image, String name, long pop_id) {
		return AddNamedPersonNative(image.raw(), name, pop_id);
	}
	public boolean addNamedPerson(Collection<OpenCVImage> images, String name, long pop_id) {
		FaceProfile fp = new FaceProfile(images);
		long imagePtrs[] = fp.getNativeImagePointerArray();
		return AddNamedPersonNativeMI(imagePtrs, name, pop_id);
	}

	public void removePersonFromDefaultPop(String name) {
		removePerson(name, getDefaultPopulationID());
	}
	public void removePerson(String name, long pop_id) {
		RemovePersonNative(name, pop_id);
	}

	public String[] listDefaultPopulation() {
		return listPopulation(getDefaultPopulationID());
	}
	public String[] listPopulation(long pop_id) {
		return ListPopulationNative(pop_id);
	}
	
	public long getDefaultPopulationID() {
		if ( m_defaultPopulation == -1 ) {
			System.out.println("Creating new default population");
			m_defaultPopulation = createPopulation();
		}
		return m_defaultPopulation;
	}

	public void loadPopulationAndReplaceDefault(String fileName) {
		// Stu asks:  Are we sure this will return a "new" id, even if this pop was loaded before?
		long id = loadPopulation(fileName);
		
		if ( m_defaultPopulation != -1 ) {
			DestroyPopulationNative(m_defaultPopulation);
		}
		
		m_defaultPopulation = id;
	}

	public long loadPopulation(String fileName) {
		// Stu asks:  Are we sure this will return a "new" id, even if this pop was loaded before?
		return LoadPopulationNative(fileName);
	}

	public boolean savePopulation(long popID, String filename) {
		return SavePopulationNative(popID, filename);
	}
	public  String computeImageQuality(OpenCVImage image) {
		return ComputeImageQualityNative(image.raw());
	}
	public FaceProfile makeProfile(Collection<OpenCVImage> images) {
		FaceProfile fp = new FaceProfile(images);
		long imagePtrs[] = fp.getNativeImagePointerArray();
		long firPointer = MakeNativeFIR(imagePtrs);
		if (firPointer != 0) {
			fp.setFIR_NativePointer(firPointer);
		} else {
			throw new RuntimeException("Got null native FIR pointer for image collection: " + images);
		}
		return fp;
	}
	public void releaseProfile(FaceProfile fp) {
		Long firPtr = fp.getFIR_NativePointer();
		if (firPtr != null) {
			FreeNativeFIR(firPtr);
		}
		fp.setFIR_NativePointer(null);
		// TODO - consider freeing the OpenCVImages held by profile.
	}
	public double compareProfiles(FaceProfile fp1, FaceProfile fp2) {
		long firPtr1 = fp1.getFIR_NativePointer();
		long firPtr2 = fp2.getFIR_NativePointer();
		return CompareNativeFIRs(firPtr1, firPtr2);
	}

	private native long CreatePopulationNative();
	private native long LoadPopulationNative(String filename);
	private native String MatchPersonNative(long image, long pop_id);
	private native String MatchOrAddPersonNative(long image, long pop_id);
	private native boolean AddNamedPersonNative(long image, String name, long pop_id);
	private native boolean AddPersonNative(long image, long pop_id);
	private native boolean AddNamedPersonNativeMI(long[] imageArr, String name, long pop_id);
	private native void RemovePersonNative(String name, long pop_id);
	private native void DestroyPopulationNative(long pop_id);
	private native String[] ListPopulationNative(long pop_id);	
	
	private native boolean SavePopulationNative(long pop_id, String filename);

	private native String ComputeImageQualityNative(long image);
	private native long MakeNativeFIR(long[] imageArr);
	private native void FreeNativeFIR(long fir);
	private native double CompareNativeFIRs(long fir1, long fir2);



	private String m_config;
	private long m_defaultPopulation;
	
	static
    {
	System.loadLibrary("RecognitionServerJNIWrapper_fv821");
    }
}
