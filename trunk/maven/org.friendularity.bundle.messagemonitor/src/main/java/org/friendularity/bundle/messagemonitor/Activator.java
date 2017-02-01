/*
 * Copyright 2014 the Friendularity Project
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

package org.friendularity.bundle.messagemonitor;

import javax.swing.UIManager;
import org.jflux.swing.messaging.monitor.AvroTableDemoFrame;
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
            org.slf4j.LoggerFactory.getLogger(AvroTableDemoFrame.class).error(ex.getMessage(), ex);
        }

        java.awt.EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                AvroTableDemoFrame frame = new AvroTableDemoFrame();
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
