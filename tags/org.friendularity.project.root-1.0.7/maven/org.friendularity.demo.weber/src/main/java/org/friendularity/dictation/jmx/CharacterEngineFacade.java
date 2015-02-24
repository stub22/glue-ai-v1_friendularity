/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.jmx;

import java.util.Map;

/**
 *
 * @author matt
 */
public class CharacterEngineFacade{
	private static CharacterEngineInterface myCEI;

	public static void connect(String serviceUrl){
        myCEI = new CharacterEngineInterface();
        myCEI.connect(serviceUrl);
	}

	public static void sendResponseMeaningToConvoid(final Map<String,Double> meanings, double c) {
		myCEI.sendResponseMeaningToConvoid(meanings, c);
	}
	
	public static void sendInformationToConvoid(String n, String m){
		myCEI.sendInformationToConvoid(n, m);
	}

	public static void sendHeard(String text){
		myCEI.sendHeardCue(text);
	}
}