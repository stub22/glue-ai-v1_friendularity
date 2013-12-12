package org.friendularity.cogbot;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jms.Connection;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MessageConsumer;
import javax.jms.Session;
import org.friendularity.bind.weber.cogbot.CogbotCommunicator;
import org.friendularity.weber.config.MeneConfig;
import org.friendularity.weber.services.GenRespWithConf;
import org.robokind.api.common.utils.Listener;
import org.robokind.api.messaging.DefaultMessageAsyncReceiver;
import org.robokind.api.messaging.DefaultMessageSender;
import org.robokind.api.messaging.MessageAsyncReceiver;
import org.robokind.api.messaging.MessageSender;
import org.robokind.api.messaging.RecordAsyncReceiver;
import org.robokind.api.messaging.RecordSender;
import org.robokind.api.speech.SpeechRequest;
import org.robokind.api.speech.SpeechRequestFactory;
import org.robokind.api.speechrec.SpeechRecEvent;
import org.robokind.api.speechrec.SpeechRecEventList;
import org.robokind.avrogen.speech.SpeechRequestRecord;
import org.robokind.avrogen.speechrec.SpeechRecEventListRecord;
import org.robokind.impl.messaging.JMSAvroRecordAsyncReceiver;
import org.robokind.impl.messaging.JMSAvroRecordSender;
import org.robokind.impl.messaging.JMSBytesMessageSender;
import org.robokind.impl.messaging.utils.ConnectionManager;
import org.robokind.impl.speech.PortableSpeechRequest;
import org.robokind.impl.speechrec.PortableSpeechRecEventList;

/**
 * Hello world!
 *
 */
public class App {
    private final static Logger theLogger = Logger.getLogger(App.class.getName());
    
    public static void main( String[] args ){
        Session session = getSession("192.168.0.108");
        if(session == null){
            return;
        }
        CogbotCommunicator cogbot = createCogbotComm();
        if(cogbot == null){
            return;
        }
        Destination sendDest = ConnectionManager.createDestination("speech.Request");
        Destination recDest = ConnectionManager.createDestination("speechrec.Event;  {create: always, node: {type: topic}}");
        
        MessageSender<SpeechRequest> sender = createSpeechSender(session, sendDest);
        if(sender == null){
            return;
        }
        SpeechHandler handler = new SpeechHandler(cogbot, sender);
        MessageAsyncReceiver<SpeechRecEventList> receiver = createSpeechReceiver(session, recDest);
        if(receiver == null){
            return;
        }
        receiver.addMessageListener(handler);
        try{
            sender.start();
            receiver.start();
        }catch(Exception ex){
            theLogger.log(Level.SEVERE, "Error starting message receiver.", ex);
            return;
        }
        //SpeechRequest req = new PortableSpeechRequest("", "", TimeUtils.now(), "hello how are you?");
        //handler.handleEvent(req);
    }
    
    private static Session getSession(String ip){
        Connection con = 
                ConnectionManager.createConnection(
                "admin", "admin", "client1", "test", "tcp://" + ip + ":5672");
        try{
            con.start();
            return con.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        }catch(JMSException ex){
            theLogger.log(Level.SEVERE, "Error starting connection.", ex);
            return null;
        }
    }
    
    private static CogbotCommunicator createCogbotComm(){
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
        return cogbot;
    }
    
    private static MessageSender<SpeechRequest> createSpeechSender(
            Session session, Destination dest){
        DefaultMessageSender<SpeechRequest, SpeechRequestRecord> sender = 
                new DefaultMessageSender<SpeechRequest, SpeechRequestRecord>();
        JMSBytesMessageSender bytesSender = new JMSBytesMessageSender();
        bytesSender.setSession(session);
        bytesSender.setDestination(dest);
        RecordSender<SpeechRequestRecord> recSender = 
                new JMSAvroRecordSender<SpeechRequestRecord>(bytesSender);
        sender.setAdapter(new PortableSpeechRequest.MessageRecordAdapter());
        sender.setRecordSender(recSender);
        try{
            sender.start();
        }catch(Exception ex){
            theLogger.log(Level.SEVERE, "Error starting message sender.", ex);
            return null;
        }
        return sender;
    }
    
    private static MessageAsyncReceiver<SpeechRecEventList> createSpeechReceiver(
            Session session, Destination dest){
        
        DefaultMessageAsyncReceiver<SpeechRecEventList, SpeechRecEventListRecord> receiver = 
                new DefaultMessageAsyncReceiver<SpeechRecEventList, SpeechRecEventListRecord>();
        MessageConsumer consumer;
        try{
            consumer = session.createConsumer(dest);
        }catch(JMSException ex){
            theLogger.log(Level.SEVERE, "Error starting message receiver.", ex);
            return null;
        }
        RecordAsyncReceiver<SpeechRecEventListRecord> recReceiver = 
                new JMSAvroRecordAsyncReceiver<SpeechRecEventListRecord>(
                        SpeechRecEventListRecord.class, 
                        SpeechRecEventListRecord.SCHEMA$, 
                        consumer);
        receiver.setRecordReceiver(recReceiver);
        receiver.setAdapter(new PortableSpeechRecEventList.RecordMessageAdapter());
        return receiver;
    }
    
    static class SpeechHandler implements Listener<SpeechRecEventList>{
        private CogbotCommunicator myCogbot;
        private MessageSender<SpeechRequest> mySpeechSender;
        private SpeechRequestFactory myFactory;
        
        public SpeechHandler(
                CogbotCommunicator cogbot, 
                MessageSender<SpeechRequest> speechSender){
            if(cogbot == null || speechSender == null){
                throw new NullPointerException();
            }
            myCogbot = cogbot;
            mySpeechSender = speechSender;
            myFactory = new PortableSpeechRequest.Factory();
        }
        
        public void handleEvent(SpeechRecEventList event) {
            List<SpeechRecEvent> events = event.getSpeechRecEvents();
            if(events == null || events.isEmpty()){
                return;
            }
            String input = events.get(0).getRecognizedText();
            GenRespWithConf genResp = myCogbot.getResponse(input);
            String resp = genResp.getResponse();
            SpeechRequest req = myFactory.create("client", "host", resp);
            mySpeechSender.sendMessage(req);
        }
    }
}
