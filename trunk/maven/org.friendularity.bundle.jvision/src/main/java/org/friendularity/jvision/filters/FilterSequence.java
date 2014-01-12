package org.friendularity.jvision.filters;

import java.util.ArrayList;
import java.util.Iterator;
import javax.swing.JFrame;
import javax.swing.ListModel;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import org.appdapter.core.log.BasicDebugger;

import org.opencv.core.Mat;

/* 
 * An ordered sequence of filters to be applied
 */
public class FilterSequence extends BasicDebugger implements BaseFilter, ListModel {
	
	private ArrayList<BaseFilter> filters = new ArrayList<BaseFilter>();

	@Override
	public void apply(Mat in, Mat out) {
		Mat temp = new Mat();
		
		in.copyTo(temp);
		applyIndexed(temp, 0, out);
	}
	
	private void applyIndexed(Mat in, int index , Mat out){
		if(index >= filters.size()) {
			in.copyTo(out);
		} else {
			BaseFilter f = filters.get(index);
			Mat temp = new Mat();
			f.apply(in, temp);
			
			applyIndexed(temp, index + 1 , out);
		}
	}
	
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
	}

	@Override
	public void showParamUI(JFrame parent) {
		
	}

	@Override
	public String serialize() {
		throw new UnsupportedOperationException("Use RDF dont serialize firlter sequence");
	}

	@Override
	public void deserialize(String str) {
		throw new UnsupportedOperationException("Use RDF dont deserialize filter sequence");
	}

	public void showParamUIForIndex(int i) {
		filters.get(i).showParamUI(null);
	}

	public String serializeIndex(int i) {
		return filters.get(i).serialize();
	}
}
