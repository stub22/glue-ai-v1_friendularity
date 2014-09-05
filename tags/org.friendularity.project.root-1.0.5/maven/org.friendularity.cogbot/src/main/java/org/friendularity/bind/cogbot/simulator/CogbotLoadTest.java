/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.bind.cogbot.simulator;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.friendularity.bind.cogbot.service.CogbotService;
import org.friendularity.bind.cogbot.slave.CogbotResponse;

/**
 *
 * @author Stu Baurmann
 */
public class CogbotLoadTest {
	private static Logger theLogger = Logger.getLogger(CogbotLoadTest.class.getName());
    PrintWriter		myServicePW;
	Properties		myProps;
	CogbotAvatar	myAvatar;
	String			myBotID = "Bina Daxeline";

	public static void main(String args[]) {
		theLogger.info("Starting Cogbot Load Test");
		try {
			Properties meneProps = new Properties();
			meneProps.load(new FileInputStream("C:\\_hanson\\_deploy\\distro_20a\\conf\\_mene\\config.properties"));
			meneProps.setProperty("cogbot_jmx_disable", "true");
			CogbotLoadTest test = new CogbotLoadTest(meneProps);
			test.start();
		} catch (Throwable t) {
			theLogger.log(Level.SEVERE, "main caught exception", t);
		}
		theLogger.info("Finished Cogbot Load Test startup");
	}
	public CogbotLoadTest (Properties meneProps)  {
		myServicePW = makePrintWriter();
		myProps = meneProps;
		CogbotAvatar csa = CogbotService.getDefaultAvatar(meneProps);
		csa.readProperties(myProps);
		myAvatar = csa;
	}
	public void start() {
		Thread loadThread1 = new Thread(makeRunnable("thr1", 2));
		// Thread loadThread2 = new Thread(makeRunnable(csa, "thr2", 100));
		loadThread1.start();
		// loadThread2.start();
	}
	public Runnable makeRunnable(final String uniqueLabel, final int loopCnt) {
		return new Runnable() {
			public void run() {
				try {
					testDialogLoop(uniqueLabel, loopCnt);
				} catch (Throwable t) {
					theLogger.log(Level.SEVERE, "Thread with label " + uniqueLabel + " caught exception", t);
				}
			}
		};
	}
	public void testDialogLoop(String uniqueLabel, int loopCnt) {
		for (int i = 1; i <= loopCnt; i++) {
			String label = uniqueLabel + "-iter-" + i;
			long iterStartStamp = System.currentTimeMillis();
			testDialog("LoaderA " + uniqueLabel, label + "A");
			testDialog("LoaderB " + uniqueLabel, label + "B");
			testDialog("LoaderC " + uniqueLabel, label + "C");
			long iterLengthStamp = System.currentTimeMillis() - iterStartStamp;
			theLogger.info("iter " + i + " took " + iterLengthStamp + " msec");
		}
	}
	public void testDialog(String userName, String label) {
		CogbotAvatar csa = myAvatar;
		myAvatar.setLookingAt(userName);
		testResponse("hello", label);
		testResponse("who am i", label);
		testResponse("howdy", label);
		testResponse("who was napoleon", label);
		testResponse("what is my name", label);
		testResponse("what is the topic", label);
		testResponse("goodbye", label);
	}
	public void testResponse(String input, String label) {
		CogbotAvatar csa = myAvatar;
		CogbotService cs = csa.service;
		CogbotResponse elRes = cs.getCogbotResponse(csa, myServicePW, myProps, input, myBotID);
		theLogger.info("Response[input='" + input + "', label='" + label + "']: " + elRes.toString());
	}
	public static PrintWriter makePrintWriter() {
		PrintWriter csPW = new PrintWriter(new Writer() {

			public void write(char[] cbuf, int off, int len) throws IOException {
					theLogger.info(new String(cbuf, off, len));
			}

			public void flush() throws IOException {
			}

			public void close() throws IOException {
			}
		});
		return csPW;
	}
}
