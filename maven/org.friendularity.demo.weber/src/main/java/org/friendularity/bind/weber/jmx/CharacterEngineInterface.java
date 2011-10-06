/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.bind.weber.jmx;

import org.cogchar.integroid.jmxwrap.IntegroidWrapperMXBean;
import javax.management.JMX;
import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

/**
 *
 * @author matt
 */
public class CharacterEngineInterface {
	private static IntegroidWrapperMXBean myIntegroidProxy;
	private static ObjectName myIntegroidON;
	private static MBeanServerConnection myMBSC;
    private static JMXConnector jmxc;
    private static String myURL;

	public static void connect(String serviceUrl){
        myURL = serviceUrl;
		try{
			JMXServiceURL jurl = new JMXServiceURL(serviceUrl);
			jmxc = JMXConnectorFactory.connect(jurl, null);

			myMBSC = jmxc.getMBeanServerConnection();
			myIntegroidON = new ObjectName(IntegroidWrapperMXBean.INTEGROID_JMX_OBJNAME);
			myIntegroidProxy = JMX.newMXBeanProxy(myMBSC, myIntegroidON, IntegroidWrapperMXBean.class);
		}catch(Throwable t){}
	}

    public static void sendThought(String name, double str){
        try{
            myIntegroidProxy.postThoughtCue(name, str);
        }catch(Throwable t){}
    }

	/*public static void sendResponseMeaningToConvoid(String name, String input, double c) {
		final List<String> meanings = Utils.list(input.split(" "));
		final double conf = c;
		new Thread(new Runnable(){
			public void run() {
                try{
                    myIntegroidProxy.postVerbalCue(meanings, conf);
                }catch(Throwable t){
                    reconnect();
                    try{
                        myIntegroidProxy.postVerbalCue(meanings, conf);
                    }catch(Throwable tt){}
                }

			}
		}).start();
	}*/

	public static void sendInformationToConvoid(String n, String m){
		final String name = n;
		final String meaning = m;
		new Thread(){
            @Override public void run()
            {
                try{
                    myIntegroidProxy.postTextCue(name, meaning, 1.0);
                }catch(Throwable t){}
            }
		}.start();
	}
}