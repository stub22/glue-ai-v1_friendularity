package org.friendularity.webber.comm;

public interface CommunicatorMBean {
	public static String	MENE_JMX_OBJNAME = "elbot:somekey=somevalue";

    public void communicate(String s, boolean b);
    public String getLastBest();
}

