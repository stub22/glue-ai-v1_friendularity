/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.bind.weber.jmx;

/**
 *
 * @author matt
 */
public class JMXInterface {
    private static String                       myCharEngURL;
    private static Boolean                      isInitialized = false;

    public static void setURL(String url){
        myCharEngURL = url;
    }

    private static void ensureInitialized(){
        if(isInitialized){
            return;
        }
        CharacterEngineInterface.connect(myCharEngURL);
        isInitialized = true;
    }
    
	public static void sendResponseToConvoid(String respName, String response){
		ensureInitialized();
        CharacterEngineInterface.sendInformationToConvoid("TXT_"+respName, response);
	}

    public static void sendThoughtToConvoid(String thought, double str){
        ensureInitialized();
        CharacterEngineInterface.sendThought(thought, str);
    }
}
