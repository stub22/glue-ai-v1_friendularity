/*
 *  Copyright 2013 by The Cogchar Project (www.cogchar.org).
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

package org.cogchar.app.puma.web;
// import org.cogchar.bind.cogbot.main.CogbotCommunicator;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class CogbotWebClientWrapper {
	private String						myCogbotConvoUrl;
//	private CogbotCommunicator			myCogbotComm;
	
	// @Override 
	public String queryCogbot(String query, String url) {
		/*
		 * We disabled this dependency on Cogbot so that PUMA doesn't drag HTTP-client deps with it.
		 * 
		if ((myCogbotComm == null) || (!url.equals(myCogbotConvoUrl))) {
			myCogbotConvoUrl = url;
			myCogbotComm = new CogbotCommunicator(myCogbotConvoUrl);
		}
		return myCogbotComm.getResponse(query).getResponse();
		* 
		*/
		return null;
	}
}
