/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.demo.lifecycles;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.jflux.api.service.DependencySpec;
import org.jflux.api.service.ServiceLifecycle;
import org.robokind.api.speech.SpeechService;
import org.robokind.api.speech.viseme.VisemeEventNotifier;

/**
 *
 * @author eadsjr
 */
public class VisemeEventNotifierLifecycle implements ServiceLifecycle<VisemeEventNotifier> {

    private final static String theSpeechService = "speechService";
    
    private final static List<DependencySpec> theDependencySpecs = new ArrayList<DependencySpec>(Arrays.asList(
            new DependencySpec(theSpeechService, SpeechService.class.getName(), DependencySpec.Cardinality.MANDATORY_UNARY, DependencySpec.UpdateStrategy.DYNAMIC, null)));
 
    private final static String[] theClassNames = new String[]{VisemeEventNotifier.class.getName()};
    
    public List<DependencySpec> getDependencySpecs() {
        return theDependencySpecs;
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
    
}
