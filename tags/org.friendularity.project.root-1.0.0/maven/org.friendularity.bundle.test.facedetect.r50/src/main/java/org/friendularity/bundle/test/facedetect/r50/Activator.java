package org.friendularity.bundle.test.facedetect.r50;

import org.jflux.api.core.Listener;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.robokind.api.vision.ImageRegionList;
import org.robokind.api.vision.config.FaceDetectServiceConfig;
import org.robokind.api.vision.messaging.RemoteImageRegionServiceClient;
import org.robokind.client.basic.Robokind;
import org.robokind.client.basic.UserSettings;

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
                Robokind.connectImageRegionService();
        Listener<ImageRegionList> monitor = new FaceMonitor(ipAddress);
        
        regions.addImageRegionsListener(monitor);
    }

    @Override
    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
