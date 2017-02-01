package org.friendularity.bundle.demo.convo.response;

import javax.swing.UIManager;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Activator implements BundleActivator {
    private final static Logger theLogger =
            LoggerFactory.getLogger(Activator.class);

    @Override
    public void start(final BundleContext context) throws Exception {
        theLogger.info("AQServiceSwingUI Activation Begin.");

        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception ex) {
            org.slf4j.LoggerFactory.getLogger(ConvoResponseFrame.class).error(ex.getMessage(), ex);
        }

        java.awt.EventQueue.invokeLater(new Runnable() {

            @Override
            public void run() {
                ConvoResponseFrame frame = new ConvoResponseFrame();
                frame.start(context);
                frame.setVisible(true);
            }
        });
        theLogger.info("AQServiceSwingUI Activation Complete.");
    }

    @Override
    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
