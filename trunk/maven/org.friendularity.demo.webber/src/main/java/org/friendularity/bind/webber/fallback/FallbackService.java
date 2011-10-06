/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.bind.webber.fallback;

import org.friendularity.webber.services.GenRespWithConf;
import org.friendularity.webber.services.INexusService;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 *
 * @author Stu Baurmann
 */
public class FallbackService  implements INexusService {
	private Random		myRandom;
	public FallbackService() {
		myRandom = new Random();
	}
    public String getServiceName() {
        return "FALLBACK";
    }

    public List<INexusService> getChildServices() {
        return new ArrayList<INexusService>();
    }

    public boolean ignoreBatchRequest() {
        return false;
    }

	public GenRespWithConf getResponse(String input) {
		return new GenRespWithConf("FALLBACK", 5);
	}
}
