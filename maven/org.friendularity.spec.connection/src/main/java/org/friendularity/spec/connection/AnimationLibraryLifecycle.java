package org.friendularity.spec.connection;

/**
 *
 * @author
 */
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.jflux.api.service.ServiceDependency;
import org.jflux.api.service.ServiceLifecycle;
import org.mechio.api.animation.library.AnimationLibrary;
import org.mechio.api.animation.library.DefaultAnimationLibrary;

public class AnimationLibraryLifecycle implements ServiceLifecycle<AnimationLibrary> {

    private static final String theSpecName = "anmationIDSpec";
    private final static ServiceDependency[] theDependencyArray = {
        new ServiceDependency(
        theSpecName, AnimationLibrarySpec.class.getName(),
        ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP)
    };
    private final static String[] theClassNameArray = {
        AnimationLibrary.class.getName()
    };

    public AnimationLibraryLifecycle() {
    }

    @Override
    public AnimationLibrary createService(Map<String, Object> dependencyMap) {

        AnimationLibrarySpec animationLibrarySpec = (AnimationLibrarySpec) dependencyMap.get(theSpecName);

        String id = animationLibrarySpec.getId();
        AnimationLibrary animationLibrary = new DefaultAnimationLibrary(id);

        return animationLibrary;
    }

    @Override
    public List<ServiceDependency> getDependencySpecs() {
        return Arrays.asList(theDependencyArray);
    }

    @Override
    public String[] getServiceClassNames() {
        return theClassNameArray;
    }
    
    @Override
    public void disposeService(AnimationLibrary service, Map<String, Object> availableDependencies) {
    }
    
    
    @Override
    public AnimationLibrary handleDependencyChange(
            AnimationLibrary service, String changeType, String dependencyName,
            Object dependency, Map<String, Object> availableDependencies) {
        return null;
    }
}
