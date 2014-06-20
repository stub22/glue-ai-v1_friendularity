/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.weber.services;

import java.util.List;

/**
 *
 * @author matt
 */
public interface INexusService {
    public GenRespWithConf getResponse(String input);
    public String getServiceName();
    public List<INexusService> getChildServices();
    public boolean ignoreBatchRequest();
}
