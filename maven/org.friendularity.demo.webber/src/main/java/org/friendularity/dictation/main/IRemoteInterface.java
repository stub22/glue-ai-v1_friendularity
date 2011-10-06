/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.main;

import java.util.Map;

/**
 *
 * @author Eamq
 */
public interface IRemoteInterface {
    public void sendResponseMeaningToConvoid(Map<String,Double> meanings);
    public void sendInformationToConvoid(String infoName, String input);
    public void sendUtterance(String category);
	public void sendToNexus(String input, Boolean real);
}
