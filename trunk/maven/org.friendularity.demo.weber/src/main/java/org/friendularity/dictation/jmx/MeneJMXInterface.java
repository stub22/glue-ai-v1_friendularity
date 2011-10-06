/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.jmx;

import javax.management.JMX;
import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import org.cogchar.convoid.broker.IRemoteResponseInterface;
import org.friendularity.weber.comm.CommunicatorMBean;

/**
 *
 * @author matt
 */
public class MeneJMXInterface implements IRemoteResponseInterface {
	private final static String[] paramTypes = {new String("java.lang.String"), new String("boolean")};
    private CommunicatorMBean myMeneProxy;
	private ObjectName myMeneON;
	private MBeanServerConnection myMBSC;
    private static MeneJMXInterface theInterface;

    public static MeneJMXInterface getInterface(){
        if(theInterface == null){
            theInterface = new MeneJMXInterface("service:jmx:rmi:///jndi/rmi://localhost:7227/jmxrmi");
        }
        return theInterface;
    }

	public MeneJMXInterface(String serviceUrl){
		try{
			JMXServiceURL jurl = new JMXServiceURL(serviceUrl);
			JMXConnector jmxc = JMXConnectorFactory.connect(jurl, null);

			myMBSC = jmxc.getMBeanServerConnection();
			myMeneON = new ObjectName(CommunicatorMBean.MENE_JMX_OBJNAME);
			myMeneProxy = JMX.newMXBeanProxy(myMBSC, myMeneON, CommunicatorMBean.class);
		}catch(Throwable t){
			t.printStackTrace();
		}
	}

	public String getResponse() {
        try{
            return myMeneProxy.getLastBest();
        }catch(Throwable t){
            t.printStackTrace();
        }
		return "";
    }

    public String sendToNexus(String input, Boolean toCharEng){
		final Object[] params = {input, toCharEng};
		new Thread(new Runnable(){
			public void run() {
				try {
                    com(params);
				}catch (Throwable t){
                    t.printStackTrace();
                }
			}
		}).start();

		return null;
	}

    private void com(Object[] params) throws Throwable{
        myMBSC.invoke(myMeneON, "communicate", params, paramTypes);
    }
}
