/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bind.cogbot.cogsim;

import java.util.logging.Level;
import java.util.logging.Logger;
import org.friendularity.bind.cogbot.simulator.CogbotAvatar;

/**
 * @author Stu Baurmann
 */
public class CogSimBridge implements Runnable {

    private static Logger theLogger = Logger.getLogger(CogSimBridge.class.getName());
    private CogSimConf myCogSimConf;
    private DictationReciever myDictationImpl;
    final CogbotAvatar avatar;

    public CogSimBridge(CogbotAvatar av, DictationReciever dimpl, CogSimConf conf) {
        avatar = av;
        myDictationImpl = dimpl;
        myCogSimConf = conf;
        //loadCogSimConf();
    }

    public synchronized boolean isConfigured() {
        return ((myCogSimConf != null) && myCogSimConf.isConfigured());
    }

    public void run() {
        int messageIn = 60;
        while (true) {
            if (!avatar.isPolling || !avatar.isCogSimEnabled) {
                try {
                    Thread.sleep(myCogSimConf.getPollSleepTime());
                } catch (InterruptedException ex) {
                    Logger.getLogger(CogSimBridge.class.getName()).log(Level.SEVERE, null, ex);
                    break;
                }
                continue;
            }
            try {
                if (!myCogSimConf.isWaiting()) {
                    String heard = fetchCogsimHeard(false);
                    if (heard != null) {
                        heard = heard.trim();
                    }
                    if ((heard != null) && (heard.length() > 0)) {
                        theLogger.info("Heard from CogSim: [" + heard + "]");
                        myDictationImpl.receiveNetworkText(heard);
                    } else {
                        if (messageIn < 0) {
                            theLogger.info("Heard NOTHING from CogSim");
                            messageIn = 60;
                        }
                    }
                }
                messageIn--;
                Thread.sleep(myCogSimConf.getPollSleepTime());
            } catch (Throwable t) {
                theLogger.log(Level.SEVERE, "run() caught exception", t);
            }
        }
    }

    private String fetchCogsimHeard(boolean debugFlag) {
        String lastHeardTxt = null;
        if (myCogSimConf != null) {
            if (!myCogSimConf.isWaiting()) {
                CogSimOp cso = new CogSimOp(avatar, myCogSimConf, null);
                try {
                    lastHeardTxt = cso.fetchLastThingWeHeard(debugFlag);
                } catch (Throwable t) {
                    theLogger.log(Level.WARNING, "Failed to fetch cogsim-last-heard", t);
                }
            }
        }
        return lastHeardTxt;
    }
}
