/*
 * Copyright 2013 Hanson Robokind LLC.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.friendularity.bundle.messageplayer;

import javax.swing.UIManager;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.rwshop.swing.messaging.player.PlayerFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Activator implements BundleActivator {
    private final static Logger theLogger =
            LoggerFactory.getLogger(Activator.class);

    public void start(final BundleContext context) throws Exception {
        theLogger.info("AQServiceSwingUI Activation Begin.");
        
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception ex) {
            java.util.logging.Logger.getLogger(
                    PlayerFrame.class.getName()).log(
                    java.util.logging.Level.SEVERE, null, ex);
        }
        
        java.awt.EventQueue.invokeLater(new Runnable() {

            public void run() {
                PlayerFrame frame = new PlayerFrame();
                frame.start(context);
                frame.setVisible(true);
            }
        });
        theLogger.info("AQServiceSwingUI Activation Complete.");
    }

    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
