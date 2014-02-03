package org.friendularity.test.facedetect.r50;

import org.jflux.api.core.Listener;
import org.mechio.api.vision.ImageRegionList;
import org.mechio.api.vision.config.FaceDetectServiceConfig;
import org.mechio.api.vision.messaging.RemoteImageRegionServiceClient;
import org.mechio.client.basic.MechIO;
import org.mechio.client.basic.UserSettings;

/**
 * Face detection demo.
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */
public class App {
    public static void main(String[] args) {
        //Set the ip address of the robot below:
        String ipAddress = "127.0.0.1";
        UserSettings.setImageRegionAddress(ipAddress);
        UserSettings.setImageRegionId("0");
        
        RemoteImageRegionServiceClient<FaceDetectServiceConfig> regions =
                MechIO.connectImageRegionService();
        Listener<ImageRegionList> monitor = new FaceMonitor(ipAddress);
        
        regions.addImageRegionsListener(monitor);
    }
}
