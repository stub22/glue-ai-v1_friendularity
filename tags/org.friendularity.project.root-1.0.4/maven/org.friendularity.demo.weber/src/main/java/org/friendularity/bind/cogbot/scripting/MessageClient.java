/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.bind.cogbot.scripting;

import java.util.logging.Level;
import java.util.logging.Logger;
import org.friendularity.bind.cogbot.jmx.CogbotJMXClient;
import org.friendularity.bind.cogbot.simulator.CogbotAvatar;

/**
 *
 * @author Administrator
 */
public class MessageClient {

   // static public CogbotJMXClient cogbotJMXClient;
    CogbotAvatar avatar;
    MessageClient(CogbotAvatar av) {
        avatar = av;
        try {
            //cogbotJMXClient = new CogbotJMXClient(CogbotJMXClient.serviceURL);
        } catch (Throwable ex) {
            Logger.getLogger(MessageClient.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
