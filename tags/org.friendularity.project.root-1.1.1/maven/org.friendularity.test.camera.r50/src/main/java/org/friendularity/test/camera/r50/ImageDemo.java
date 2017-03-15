package org.friendularity.test.camera.r50;

import java.awt.EventQueue;
import javax.swing.JFrame;
import javax.swing.UIManager;
import org.mechio.api.vision.config.CameraServiceConfig;
import org.mechio.api.vision.messaging.RemoteImageServiceClient;
import org.mechio.client.basic.MechIO;
import org.mechio.client.basic.UserSettings;
import org.slf4j.LoggerFactory;

/**
 * Camera service demo.
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */
public class ImageDemo extends JFrame {
    public ImageDemo() {
        //Set the ip address of the robot below:
//        String ipAddress = "127.0.0.1";
//        UserSettings.setCameraAddress(ipAddress);
        UserSettings.setCameraId("0");

        RemoteImageServiceClient<CameraServiceConfig> images =
                MechIO.connectCameraService();
        ImageMonitor monitor = new ImageMonitor();

        add(monitor);

        images.addImageListener(monitor);
    }

    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception ex) {
            LoggerFactory.getLogger(ImageDemo.class).error(ex.getMessage(), ex);
        }

        EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                ImageDemo frame = new ImageDemo();
                frame.setVisible(true);
            }
        });
    }
}
