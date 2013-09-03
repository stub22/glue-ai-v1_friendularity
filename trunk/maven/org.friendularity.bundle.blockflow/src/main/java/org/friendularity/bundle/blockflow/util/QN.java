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
package org.friendularity.bundle.blockflow.util;

/**
 *  a bunch of utility functions for working with RDFish stuff
 * Notably it converts a qname to a global name in our normal namespaces
 *  
 * @author Annie
 */
public class QN {
	public static String flo(String qname)
	{
		return "http://www.friendularity.org/ontology/flo#".concat(qname);
	}
	
	public static String rdf(String qname)
	{
		return "http://www.w3.org/1999/02/22-rdf-syntax-ns#".concat(qname);
	}
	
	public static String rdfs(String qname)
	{
		return "http://www.w3.org/2000/01/rdf-schema#".concat(qname);	
	}
}
