/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.jmx;

/**
 *
 * @author matt
 */
public class NexusInterface {
	private static MeneJMXInterface myNexusInterface;

	public static void connect(String serviceUrl){
        myNexusInterface = new MeneJMXInterface(serviceUrl);
	}

	public static String sendToNexus(String input, Boolean toCharEng){
		return myNexusInterface.sendToNexus(input, toCharEng);
	}
}
