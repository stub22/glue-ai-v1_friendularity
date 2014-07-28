package org.friendularity.bundle.discovery;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {
    @Override
    public void start(BundleContext context) throws Exception {
        java.awt.EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                SelectorFrame frame = new SelectorFrame();
                Discoverer discoverer = new Discoverer();
                Thread discoThread = new Thread(discoverer);
                
                frame.initialize(discoverer);
                discoThread.start();
                frame.setVisible(true);
                
                Thread beaconThread = new Thread(new Beacon());
                beaconThread.start();
            }
        });
    }

    @Override
    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
