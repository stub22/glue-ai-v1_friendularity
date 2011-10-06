/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.bind.webber.ask;

import org.friendularity.webber.services.GenRespWithConf;
import org.friendularity.webber.services.INexusService;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author matt
 */
public class AskDotComService implements INexusService{
    private String myInput = null;

    public AskDotComService(){}
    public AskDotComService(String input){
        myInput = input;
    }

    public GenRespWithConf getResponse(String input) {
        if(myInput != null){
            input = myInput;
        }
        return AskAskDotCom.getBestStandard(input);
    }

    public String getServiceName(){
        return "ASK_STANDARD";
    }

    public List<INexusService> getChildServices() {
        return new ArrayList<INexusService>();
    }

    public boolean ignoreBatchRequest() {
        return false;
    }
}
