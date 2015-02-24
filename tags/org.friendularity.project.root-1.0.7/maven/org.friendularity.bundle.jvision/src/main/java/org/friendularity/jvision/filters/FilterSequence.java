package org.friendularity.jvision.filters;

import java.util.ArrayList;
import java.util.Iterator;
import javax.swing.JFrame;
import javax.swing.ListModel;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import org.appdapter.core.log.BasicDebugger;
import org.friendularity.jvision.broker.ImageStreamConsumer;
import org.friendularity.jvision.broker.ImageStreamImage;
import org.friendularity.jvision.broker.SimpleImageStreamProducer;

import org.opencv.core.Mat;

/* 
 * An ordered sequence of filters to be applied
 */
public class FilterSequence implements ListModel, ImageStreamConsumer {
	
	private ArrayList<BaseFilter> filters = new ArrayList<BaseFilter>();
	
	public void addOrReplaceByClass(BaseFilter f) {
		for(int i = filters.size() - 1 ; i >= 0 ; i--){

			if(f.getClass().isInstance(filters.get(i))) {
				filters.set(i, f);
				return;
			}
		}
		filters.add(f);
		filterSequenceChanged();
	}
	
	public void removeByClass(BaseFilter f) {
		for(int i = filters.size() - 1 ; i >= 0 ; i--){

			if(f.getClass().isInstance(filters.get(i))) {
				filters.remove(i);
			}
		}		
		filterSequenceChanged();
	}

	/**
	 * remove the sourceth filter and insert it at index target
	 * note that index is after the removal, so you have to compensate if it's after
	 * 
	 * @param source
	 * @param target 
	 */
	public void move(int source, int target) {
		BaseFilter f = filters.get(source);
		filters.remove(source);
		filters.add(target, f);
		filterSequenceChanged();  // ANNIE - added this 2/19/2014 
	}

	public void add(BaseFilter f) {
		filters.add(f);
		filterSequenceChanged();
	}
	
	public void remove(int index) {
		filters.remove(index);
		filterSequenceChanged();
	}
	
	@Override
	public String toString() {
		return "filter_sequence"; 
	}
	
	// ========================  Interface ListModel ==========================
	//
	// These are intended for Swing interface, probably not what you want.
	
	private ArrayList<ListDataListener> listModelListeners = new ArrayList<ListDataListener>();
	
	@Override
	public int getSize() {
		return filters.size();
	}
	
	@Override
	public Object getElementAt(int index) {
		return filters.get(index).toString();
	}

	@Override
	public void addListDataListener(ListDataListener l) {
		listModelListeners.add(l);
	}

	@Override
	public void removeListDataListener(ListDataListener l) {
		listModelListeners.remove(l);
	}

	private void filterSequenceChanged() {
		ListDataEvent lde = new ListDataEvent(this, ListDataEvent.CONTENTS_CHANGED, 0, filters.size() - 1);
		for(Iterator<ListDataListener> i = listModelListeners.iterator() ; i.hasNext() ; )
		{
			i.next().contentsChanged(lde);
		}
		
		rewireImageChain();
	}

	public void showParamUIForIndex(int i) {
		filters.get(i).showParamUI(null);
	}

	public String serializeIndex(int i) {
		return filters.get(i).serialize();
	}

	@Override
	public void setConsumedImage(ImageStreamImage img) {
		if(filters.isEmpty())
			outputLocation.setConsumedImage(img);
		else
			filters.get(0).setConsumedImage(img);
	}

	@Override
	public void setConsumedMessage(String string) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	@Override
	public void sourceIsEnding() {
		// TODO
		throw new UnsupportedOperationException("Not supported yet.");
	}

	private ImageStreamConsumer outputLocation = null;
	
	public void sendOutputTo(SimpleImageStreamProducer outputProducer) {
		outputLocation = outputProducer;
	}

	private void rewireImageChain() {
		for(Iterator<BaseFilter>i = filters.iterator() ; i.hasNext() ; ) {
			i.next().removeAllConsumers();
		}
		
		for(int i = 0; i < filters.size() - 1 ; i++)
			filters.get(i).addConsumer(filters.get(i+1));
		
		filters.get(filters.size()-1).addConsumer(outputLocation);
	}
}
