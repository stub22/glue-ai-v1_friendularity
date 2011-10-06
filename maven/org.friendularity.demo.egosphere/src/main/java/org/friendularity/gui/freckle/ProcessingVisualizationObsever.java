package org.friendularity.gui.freckle;

import org.friendularity.ancient.FaceRecServer.ProcessingObserver;
import org.cogchar.integroid.boot.ThreadAwareObject;
import org.cogchar.vision.OpenCVImage;

public class ProcessingVisualizationObsever extends ThreadAwareObject implements ProcessingObserver {
	//Received Requests
	public void ReceivedCreatePopulation() {}
	public void ReceivedMatchPerson(OpenCVImage image, long pop_id) {}
	public void ReceivedMatchOrAddPerson(OpenCVImage image, long pop_id) {}
	public void ReceivedAddPerson(OpenCVImage image, long pop_id) {}
	public void ReceivedRemovePerson(String name, long pop_id) {}
	public void ReceivedDestroyPopulation(long pop_id) {}
	public void ReceivedListPopulation(long pop_id) {}

	// Processed answers
	public void FinishCreatePopulation(long pop_id) {} 
	public void FinishMatchPerson(OpenCVImage image, long pop_id, String name) {}
	public void FinishMatchOrAddPerson(OpenCVImage image, long pop_id, String name) {}
	public void FinishAddPerson(OpenCVImage image, long pop_id, String name) {}
	public void FinishRemovePerson(String name, long pop_id) {}
	public void FinishDestroyPopulation(long pop_id) {}
	public void FinishListPopulation(long pop_id, int nNames) {}	

}
