package org.friendularity.test.camera.r50;

import java.awt.EventQueue;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JFrame;
import javax.swing.UIManager;
import org.robokind.api.vision.config.CameraServiceConfig;
import org.robokind.api.vision.messaging.RemoteImageServiceClient;
import org.robokind.client.basic.Robokind;
import org.robokind.client.basic.UserSettings;

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
                Robokind.connectCameraService();
        ImageMonitor monitor = new ImageMonitor();
        
        add(monitor);
        
        images.addImageListener(monitor);
    }
    
    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception ex) {
            Logger.getLogger(ImageDemo.class.getName()).log(Level.SEVERE, null, ex);
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
