package org.friendularity.bundle.test.facedetect.r50;

import org.jflux.api.core.Listener;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.mechio.api.vision.ImageRegionList;
import org.mechio.api.vision.config.FaceDetectServiceConfig;
import org.mechio.api.vision.messaging.RemoteImageRegionServiceClient;
import org.mechio.client.basic.MechIO;
import org.mechio.client.basic.UserSettings;

public class Activator implements BundleActivator {

    @Override
    public void start(BundleContext context) throws Exception {
        String envVarKey =
                "org.friendularity.bundle.test.facedetect.r50.connections";
        String ipAddress = System.getProperty(
                envVarKey, System.getenv(envVarKey));
        UserSettings.setImageRegionAddress(ipAddress);
        UserSettings.setImageRegionId("0");
        
        RemoteImageRegionServiceClient<FaceDetectServiceConfig> regions =
                MechIO.connectImageRegionService();
        Listener<ImageRegionList> monitor = new FaceMonitor(ipAddress);
        
        regions.addImageRegionsListener(monitor);
    }

    @Override
    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
