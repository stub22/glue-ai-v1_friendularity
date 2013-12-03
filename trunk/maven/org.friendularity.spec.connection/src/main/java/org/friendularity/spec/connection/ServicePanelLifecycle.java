package org.friendularity.spec.connection;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import javax.swing.UIManager;
import org.jflux.api.service.ServiceDependency;
import org.jflux.api.service.ServiceLifecycle;
import org.osgi.framework.BundleContext;
import org.rwshop.swing.common.lifecycle.ServicesFrame;

/**
 *
 * @author Major Jacquote II <mjacquote@gmail.com>
 */
public class ServicePanelLifecycle implements ServiceLifecycle<ServicesFrame> {

    private static BundleContext myContext;
    private static ServicesFrame sf;
    private final static String contextDependency = "bundlecontext_dep";
    private final static ServiceDependency[] theDependencyArray = {
        new ServiceDependency(contextDependency, BundleContextSpec.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP)
    };
    private final static String[] theClassNameArray = {
        ServicesFrame.class.getName()
    };

    @Override
    public List<ServiceDependency> getDependencySpecs() {
        return Arrays.asList(theDependencyArray);
    }

    @Override
    public String[] getServiceClassNames() {
        return theClassNameArray;
    }

    @Override
    public ServicesFrame createService(Map<String, Object> dependencyMap) {

        myContext = ((BundleContextSpec) dependencyMap.get(contextDependency)).getContext();
        setLookAndFeel();
        startServicePanel(myContext);

        return sf;

    }

    @Override
    public void disposeService(
            ServicesFrame service,
            Map<String, Object> availableDependencies) {

        if (service != null) {
            service=null;
        }
    }

    @Override
    public ServicesFrame handleDependencyChange(
            ServicesFrame service, String changeType,
            String dependencyName, Object dependency,
            Map<String, Object> availableDependencies) {
        return null;

    }

    private void startServicePanel(final BundleContext context) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                sf = new ServicesFrame();
                sf.setBundleContext(context);
                sf.setVisible(true);
            }
        });
    }
    
    private void setLookAndFeel() {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception ex) {
        }
    }
}
