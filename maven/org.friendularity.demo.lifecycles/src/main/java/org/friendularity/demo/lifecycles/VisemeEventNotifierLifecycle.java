/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.demo.lifecycles;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.jflux.api.service.ServiceDependency;
import org.jflux.api.service.ServiceLifecycle;
import org.mechio.api.speech.SpeechService;
import org.mechio.api.speech.viseme.VisemeEventNotifier;

/**
 *
 * @author eadsjr
 */
public class VisemeEventNotifierLifecycle implements ServiceLifecycle<VisemeEventNotifier> {

    private final static String theSpeechService = "speechService";
    
    private final static List<ServiceDependency> theServiceDependencys = new ArrayList<ServiceDependency>(Arrays.asList(
            new ServiceDependency(theSpeechService, SpeechService.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY, ServiceDependency.UpdateStrategy.DYNAMIC, null)));
 
    private final static String[] theClassNames = new String[]{VisemeEventNotifier.class.getName()};
    
    public List<ServiceDependency> getServiceDependencys() {
        return theServiceDependencys;
    }

    public VisemeEventNotifier createService(Map<String, Object> services) {
        SpeechService speech = (SpeechService)services.get(theSpeechService);
        VisemeEventNotifier ven = new VisemeEventNotifier();
        speech.addSpeechEventListener(ven);
        return ven;
    }

    public VisemeEventNotifier handleDependencyChange(VisemeEventNotifier t, String changeType,
            String dependencyName, Object dependency, Map<String,Object> availableDependencies) {
        return t;
    }

    public void disposeService(VisemeEventNotifier t, Map<String, Object> services) {
    }

    public String[] getServiceClassNames() {
        return theClassNames;
    }

    @Override
    public List<ServiceDependency> getDependencySpecs() {
        return theServiceDependencys;
    }
    
}
