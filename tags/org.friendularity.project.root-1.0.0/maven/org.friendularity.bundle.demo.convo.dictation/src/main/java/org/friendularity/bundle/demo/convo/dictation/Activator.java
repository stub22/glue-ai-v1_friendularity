package org.friendularity.bundle.demo.convo.dictation;

import org.cogchar.bundle.demo.dictation.ui.DictationFrame;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

    @Override
    public void start(BundleContext context) throws Exception {
        DictationFrame.main(null);
    }

    @Override
    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
