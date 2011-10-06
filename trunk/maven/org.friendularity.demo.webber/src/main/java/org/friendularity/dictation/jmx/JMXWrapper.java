/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.jmx;

import org.friendularity.dictation.main.IRemoteInterface;
import java.util.Map;

/**
 *
 * @author Eamq
 */
public class JMXWrapper implements IRemoteInterface{
    public void sendResponseMeaningToConvoid(Map<String,Double> meanings){
        JMXInterface.sendResponseMeaningToConvoid(meanings);
    }

    public void sendInformationToConvoid(String infoName, String input){
        JMXInterface.sendInformationToConvoid(infoName, input);
    }

    public void sendUtterance(String category){
        JMXInterface.sendUtterance(category);
    }

	public void sendToNexus(String input, Boolean real){
        JMXInterface.sendToNexus(input, real);
    }

	public void sendHeard(String text){
		JMXInterface.sendHeard(text);
	}
}
