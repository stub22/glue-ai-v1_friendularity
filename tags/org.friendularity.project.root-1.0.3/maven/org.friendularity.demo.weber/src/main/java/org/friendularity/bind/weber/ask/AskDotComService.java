/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.bind.weber.ask;

import org.friendularity.weber.services.GenRespWithConf;
import org.friendularity.weber.services.INexusService;
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
