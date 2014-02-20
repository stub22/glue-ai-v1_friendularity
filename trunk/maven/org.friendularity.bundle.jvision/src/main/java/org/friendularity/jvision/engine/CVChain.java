 /*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.friendularity.jvision.engine;

import org.friendularity.jvision.filters.FilterInfoManager;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import java.util.HashMap;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.ListModel;
import org.friendularity.jvision.broker.ImageFlavorNotAvailable;
import org.friendularity.jvision.broker.ImageStreamBroker;
import org.friendularity.jvision.broker.ImageStreamConsumer;
import org.friendularity.jvision.broker.ImageStreamImage;
import org.friendularity.jvision.broker.SimpleImageStreamProducer;
import org.friendularity.jvision.filters.BaseFilter;
import org.friendularity.jvision.filters.FilterInfo;
import org.friendularity.jvision.filters.FilterSequence;
import org.friendularity.jvision.gui.CVChainControl;
import org.friendularity.jvision.gui.FilterBox;
import org.opencv.core.Mat;

/**
 *  A chain of filters.
 * 
 * The chain has a name and source.
 * The chain will register with the source, and apply the filters in order to each image.
 * The final output will be published as <name>.out
 * There's pieces of code to have it optionally publish all the intermediate steps, but this isn't 
 * finished yet
 * 
 * CVChainManager should be used to build these. The constructor has to be package access for
 * CVChainManager, but others shouldn't use it.
 * 
 * @author Annie
 */
public class CVChain implements ImageStreamConsumer {
	protected String chainName;
	protected boolean intermediatesVisible;
	protected String source;
	protected FilterSequence filters;
	protected SimpleImageStreamProducer outputProducer;
	
	/**
	 * The only thing that should call this is CVChainManager
	 * 
	 * @param chainName
	 * @param intermediatesVisible
	 * @param source 
	 */
	CVChain(String chainName, boolean intermediatesVisible, String source) {
		this.chainName = chainName;
		this.intermediatesVisible = intermediatesVisible;
		this.source = source;
		filters = new FilterSequence();

		init();
	}

	public CVChain(Model M, Resource cvchain) {
		Statement s = M.getProperty(cvchain, M.createProperty(JVisionRDF.RDF_PREFIX + "label"));
		
		this.chainName = s.getObject().asLiteral().getString();
		s = M.getProperty(cvchain, M.createProperty(JVisionRDF.FLO_PREFIX + "source"));
		
		this.source = s.getObject().asLiteral().getString();
		filters = new FilterSequence();
		
		this.intermediatesVisible = false;
				
		init();
		
		TreeMap<Integer, FilterInfo>filtersToAdd = new TreeMap<Integer, FilterInfo>();
		TreeMap<Integer, String>filterParms = new TreeMap<Integer, String>();
		
		Property p = M.createProperty(JVisionRDF.RDF_PREFIX + "type");
		Resource o = M.createResource(JVisionRDF.FLO_PREFIX + "FilterInstance");
		for(ResIterator i = M.listResourcesWithProperty(p, o);
				i.hasNext(); ) {
			Resource filterInstance = i.next();
			
			FilterInfo ff;
			String type = filterInstance.getProperty(M.createProperty(JVisionRDF.FLO_PREFIX + "filterType")).getObject().asLiteral().getString();
			int index = filterInstance.getProperty(M.createProperty(JVisionRDF.FLO_PREFIX + "index")).getObject().asLiteral().getInt();
			Resource itsChain = filterInstance.getProperty(M.createProperty(JVisionRDF.FLO_PREFIX + "inChain")).getObject().asResource();
			if(itsChain.equals(cvchain)) {
				filtersToAdd.put(index, FilterInfoManager.getFilterInfo(type));
				String parms = filterInstance.getProperty(M.createProperty(JVisionRDF.FLO_PREFIX + "filterParameters")).getObject().asLiteral().getString();
				filterParms.put(index, parms);
			}
		}
		
		// TreeMaps keyset iterator guarantess ascending order
		for(Iterator<Integer>i = filtersToAdd.keySet().iterator() ; i.hasNext() ; ) {
			Integer ii = i.next();
			BaseFilter f = filtersToAdd.get(ii).createInstance();
			f.deserialize(filterParms.get(ii));
			filters.add(f);
		}
	}
	
	/**
	 * wire up the pub sub.
	 * 
	 */
	private void init() {
		Logger.getLogger(CVChain.class.getName()).log(Level.INFO, "about to wire up imagestream");
		ImageStreamBroker.getDefaultImageStreamBroker().alwaysAddImageStreamConsumer(source, this);
		outputProducer = new SimpleImageStreamProducer(getOutName());
		ImageStreamBroker.getDefaultImageStreamBroker().addImageStreamProducer(outputProducer);
		Logger.getLogger(CVChain.class.getName()).log(Level.INFO, "done wiring up imagestream");
		filters.sendOutputTo(outputProducer);
	}

	public String getName() {
		return chainName;
	}

	/**
	 * 
	 * @return the name of the source
	 */
	public String getSource() {
		return source;
	}

	/**
	 * 
	 * @return the name we publish our output as
	 */
	public String getOutName() {
		return chainName + ".out";
	}

	/**
	 * This is a tad iffy since it's letting a mutable structure out of the bag
	 * 
	 * @return the FilterSequence.
	 */
	public ListModel getFilterSequence() {
		return filters;
	}

	public void unwire() {
		ImageStreamBroker.getDefaultImageStreamBroker().removeImageStreamConsumerAllStreams(this);
		ImageStreamBroker.getDefaultImageStreamBroker().removeImageStreamProducer(outputProducer.getSourceName());
	}
	
	// =============== ImageStreamConsumer ==========================
	@Override
	public void setConsumedImage(ImageStreamImage in) {
		filters.setConsumedImage(in);

			// TODO make the last element forward to outputProducer
		// TODO wire up the producer chain
		   
	}

	@Override
	public void setConsumedMessage(String string) {
		
	}

	@Override
	public void sourceIsEnding() {

	}

}
