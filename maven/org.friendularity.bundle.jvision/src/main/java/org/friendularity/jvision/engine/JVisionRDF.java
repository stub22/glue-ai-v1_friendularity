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

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;

/**
 *
 * @author Annie
 */
public class JVisionRDF {
	public static final String CV_PREFIX = "http://www.friendularity.org/cv#";
	public static final String FLO_PREFIX = "http://www.friendularity.org/ontology/flo#";
	public static final String RDF_PREFIX = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

	public static Model createDefaultJVisionModel() {
		Model M = ModelFactory.createDefaultModel();
		M.setNsPrefix("flo", JVisionRDF.FLO_PREFIX);
		M.setNsPrefix("cv", JVisionRDF.CV_PREFIX);
		M.setNsPrefix("rdf", JVisionRDF.RDF_PREFIX);
		return M;
	}
}
