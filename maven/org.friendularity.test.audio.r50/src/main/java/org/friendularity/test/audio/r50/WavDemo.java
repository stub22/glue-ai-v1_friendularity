package org.friendularity.test.audio.r50;

import javax.jms.Connection;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Session;
import org.jflux.impl.messaging.rk.JMSAvroRecordSender;
import org.jflux.impl.messaging.rk.JMSBytesMessageSender;
import org.jflux.impl.messaging.rk.utils.ConnectionManager;
import org.mechio.impl.audio.config.WavPlayerConfigRecord;

/**
 * WAV Player demo.
 * @author Amy Jessica Book <jgpallack@gmail.com>
 */
public class WavDemo 
{
    private static Connection theConnection;
    private static Session theSession;
    private static Destination theDestination;
    
    public static void main(String[] args)
    {
        // The WAV must be on the robot in order to play it
        // Replace this bogus path with the actual one
        // A decent file for testing is /usr/share/sounds/alsa/Front_Left.wav
        String wavPath = "/path/to/wav/on/robot.wav";
        
        // Replace this with the IP address of the robot
        String ipAddress = "127.0.0.1";
        
        WavPlayerConfigRecord config = configureStart(wavPath);
        connect(ipAddress);
        JMSAvroRecordSender<WavPlayerConfigRecord> sender = makeSender();
        sender.sendRecord(config);
        disconnect();
    }
    
    private static WavPlayerConfigRecord configureStart(String wavPath) {
        WavPlayerConfigRecord.Builder builder =
                WavPlayerConfigRecord.newBuilder();
        
        // Set the WAV path
        builder.setWavLocation(wavPath);
        
        // Will initiate playback if player ID ends with _start
        // Will kill playback if player ID ends with _stop
        builder.setWavPlayerId("testPlayer_start");
        
        // The rest of these fields are unused, but let's initialize them anyway
        builder.setStartDelayMillisec(0);
        builder.setStartTimeMicrosec(0);
        builder.setStopTimeMicrosec(0);
        
        return builder.build();
    }
    
    private static WavPlayerConfigRecord configureStop(String wavPath) {
        // Not used in this demo, but use this to stop playback in progress
        
        WavPlayerConfigRecord.Builder builder =
                WavPlayerConfigRecord.newBuilder();
        
        // Set the WAV path
        builder.setWavLocation(wavPath);
        
        // Will initiate playback if player ID ends with _start
        // Will kill playback if player ID ends with _stop
        builder.setWavPlayerId("testPlayer_stop");
        
        // The rest of these fields are unused, but let's initialize them anyway
        builder.setStartDelayMillisec(0);
        builder.setStartTimeMicrosec(0);
        builder.setStopTimeMicrosec(0);
        
        return builder.build();
    }
    
    private static void connect(String ipAddress) {
        // Generate the Qpid objects we'll need in order to send a message
        
        // This is where the player looks for config messages
        String destString =
                "wavPlayerEvent; {create: always, node: {type: topic}}";
        
        try {
            theConnection = ConnectionManager.createConnection(
                    "admin", "admin", "client1", "test",
                    "tcp://" + ipAddress + ":5672");
            theDestination = ConnectionManager.createDestination(destString);
            try {
                theSession =
                        theConnection.createSession(
                                false, Session.CLIENT_ACKNOWLEDGE);
                theConnection.start();
            } catch(JMSException ex) {
                System.out.println(
                        "Unable to create Session: " + ex.getMessage());
            }
        } catch(Exception e) {
            System.out.println("Connection error: " + e.getMessage());
            
            disconnect();
        }
    }
    
    private static void disconnect() {
        // Kill the session and connection, in that order
        
        if(theSession != null) {
            try {
                theSession.close();
            } catch(JMSException ex) {
            }
        }

        if(theConnection != null) {
            try {
                theConnection.close();
            } catch(JMSException ex) {
            }
        }

        theConnection = null;
        theDestination = null;
        theSession = null;
    }
    
    private static JMSAvroRecordSender<WavPlayerConfigRecord> makeSender() {
        // Make a record sender to send the WAV player config
        
        JMSBytesMessageSender msgSender = new JMSBytesMessageSender();
        msgSender.setSession(theSession);
        msgSender.setDestination(theDestination);
        msgSender.openProducer();
        
        return new JMSAvroRecordSender<WavPlayerConfigRecord>(msgSender);
    }
}
