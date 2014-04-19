package org.friendularity.bind.weber.cogbot;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Properties;

import org.friendularity.weber.config.MeneConfig;
import org.friendularity.weber.services.GenRespWithConf;


public class CogbotCommunicationTest {

	public static void main(String[] args) {
		System.out.println("Creating a test config");
		Properties config = new Properties();
		String testUser = "Test user";
		String id = testUser.replace(" ", "");
		try {
			config.load(new FileReader("./resources/config.properties"));
		} catch (FileNotFoundException e) {
			System.out.println("No config file found using defaults.");
			config.setProperty("reset_phrase", "reload aiml");
			String urlStr = "http://10.10.10.190:5580/chat?";
			config.setProperty("elbot_url_local", urlStr);
			config.setProperty("elbot_url_remote", urlStr);
			config.setProperty("id", id);
			config.setProperty("id_key", id);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		MeneConfig mc = new MeneConfig();
		mc.load_configuration(config);
		System.out.println("Creating cogbot. Sending hello");
		CogbotCommunicator cogbot = new CogbotCommunicator(mc);
		cogbot.setBotProperty("username", testUser);
		GenRespWithConf r = cogbot.getResponse("Hello");
	  System.out.println("bot> " + r.getResponse());
		boolean done = false;
		BufferedReader isr = new BufferedReader( new InputStreamReader(System.in));
		while (!done) {
		  final String input;
			try {
				System.out.print(" cogbot.LastUser> ");
				input = isr.readLine();
			  r = cogbot.getResponse(input);
			  System.out.println("bot> " + r.getResponse());
			} catch (IOException e) {
				e.printStackTrace();				
			}
		}
	}
}
