/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.bind.webber.jmx;

import org.friendularity.webber.comm.Communicator;
import java.lang.management.ManagementFactory;
import javax.management.MBeanServer;
import javax.management.ObjectName;

/**
 *
 * @author matt
 */
public class JMXListener {
	private static MBeanServer myMBS;
	private static String OBJ_NAME = "elbot:somekey=somevalue";

    public static void createAndRegister(Communicator com){
        try{
            myMBS = ManagementFactory.getPlatformMBeanServer();
            ObjectName name = new ObjectName(OBJ_NAME);
            myMBS.registerMBean(com, name);
        }
        catch(Throwable e){
                e.printStackTrace();
        }
	}
}
