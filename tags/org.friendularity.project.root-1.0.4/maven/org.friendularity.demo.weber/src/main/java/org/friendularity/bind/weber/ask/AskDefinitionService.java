/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.bind.weber.ask;

import org.friendularity.weber.services.BatchServiceRequest;
import org.friendularity.weber.services.GenRespWithConf;
import org.friendularity.weber.services.INexusService;
import org.friendularity.weber.services.ServiceRequestThread;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author matt
 */
public class AskDefinitionService implements INexusService{

    public GenRespWithConf getResponse(String input) {
        String newInput = "";
        if(AskAskDotCom.myConfig != null)
            newInput = AskAskDotCom.myConfig.getFormatter("question").format(input);

        if(newInput.equals(input))
            return new GenRespWithConf();

        List<INexusService> services = new ArrayList<INexusService>();
        services.add(new AskDotComService(newInput));
        services.add(new AskDotComService(input));
        BatchServiceRequest bsr = new BatchServiceRequest(services, input);
        for(ServiceRequestThread srt : bsr.getCompletedRequests()){
            if(srt.getService() == services.get(0) && srt.getResponse().getConfidence() >= 9){
                return srt.getResponse();
            }
        }
        return bsr.getBestResponse();
    }

    public String getServiceName(){
        return "ASK_DEFINE";
    }

    public List<INexusService> getChildServices() {
        return new ArrayList<INexusService>();
    }

    public boolean ignoreBatchRequest() {
        return false;
    }
}
