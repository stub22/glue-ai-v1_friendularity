package org.friendularity.test.camera.r50;

import org.jflux.api.core.Listener;
import org.robokind.api.vision.ImageRegionList;
import org.robokind.api.vision.config.FaceDetectServiceConfig;
import org.robokind.api.vision.messaging.RemoteImageRegionServiceClient;
import org.robokind.client.basic.Robokind;
import org.robokind.client.basic.UserSettings;

/**
 * Face detection demo.
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */
public class App {
    public static void main(String[] args) {
        String ipAddress = "192.168.2.126";
        UserSettings.setImageRegionAddress(ipAddress);
        UserSettings.setImageRegionId("0");
        
        RemoteImageRegionServiceClient<FaceDetectServiceConfig> regions =
                Robokind.connectImageRegionService();
        Listener<ImageRegionList> monitor = new FaceMonitor(ipAddress);
        
        regions.addImageRegionsListener(monitor);
    }
}