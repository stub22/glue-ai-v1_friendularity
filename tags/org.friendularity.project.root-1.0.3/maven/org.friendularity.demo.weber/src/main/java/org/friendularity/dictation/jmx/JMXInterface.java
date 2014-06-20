/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.jmx;

import java.util.Map;
import java.util.Properties;

/**
 *
 * @author matt
 */
public class JMXInterface {
    private static String                       myCharEngURL;
    private static String                       myNexusURL;
    private static Boolean                      isInitialized = false;

    public static void setProperties(Properties prop){
        myCharEngURL = prop.getProperty("CharacterEngineURL");
        myNexusURL = prop.getProperty("MessagingNexusURL");
    }

    public static void connect(){
        ensureInitialized();
    }

    private static void ensureInitialized(){
        if(isInitialized){
            return;
        }
        CharacterEngineFacade.connect(myCharEngURL);
        NexusInterface.connect(myNexusURL);
        isInitialized = true;
    }
	public static void sendResponseMeaningToConvoid(Map<String,Double> meanings){
        ensureInitialized();
        CharacterEngineFacade.sendResponseMeaningToConvoid(meanings, 0.6);
	}
	public static void sendInformationToConvoid(String infoName, String input){
        ensureInitialized();
        CharacterEngineFacade.sendInformationToConvoid("TXT_"+infoName, input);
	}
	public static void sendUtterance(String category){
		sendInformationToConvoid("UTTERANCE", category);
	}
	public static void sendToNexus(String input, Boolean real){
        ensureInitialized();
        NexusInterface.sendToNexus(input, real);
	}
	public static void sendHeard(String text){
		ensureInitialized();
		CharacterEngineFacade.sendHeard(text);
	}
}
