/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bundle.macro.behavior;

import java.io.IOException;
import java.util.Map;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;

import org.jflux.impl.services.rk.lifecycle.AbstractLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.utils.DescriptorListBuilder;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.osgi.framework.BundleContext;

/**
 *
 * @author eadsjr
 */
public class ActionIconBinding {
    
    ImageIconMap myBehaviorIconMap;
    ImageIconMap myAdminIconMap;
    
    public ActionIconBinding( ImageIconMap behaviorMap, ImageIconMap adminMap  ) {
        if( behaviorMap == null || adminMap == null ) {
            throw new NullPointerException();
        }
        
        myBehaviorIconMap = behaviorMap;
        myAdminIconMap = adminMap;
        
        //TODO: extract this data to a source
        myAdminIconMap.putIconImage(
                "ADMIN-Reload_Content_and_Reset",
                getImageIcon("/com/hrkind/demo/behavior/master/images/RefreshIcon_freeiconswebDOTcom.png"));
                //getImageIcon("/images/RefreshIcon_freeiconwebDOTcom.png"));
                //getImageIcon("./images/RefreshIcon_freeiconwebDOTcom.png"));
        
        myAdminIconMap.putIconImage(
                "ADMIN-Stop_Behavior",
                getImageIcon("/com/hrkind/demo/behavior/master/images/StopIcon_freeiconswebDOTcom.png"));
                //getImageIcon("./images/StopIcon_freeiconwebDOTcom.png"));
    }
    
    //TODO: this should be provided by another bundle/lifecycle
    private ImageIcon getImageIcon(String path) {
        ImageIcon icon = null;
        try {
            icon = new ImageIcon(ImageIO.read(getClass().getResourceAsStream(path)));
        }
        catch( IOException e ) {
            System.err.print(e);
            System.err.println("Image Not Found In Resources");
        }
        return icon;
    }
    
    public static class ActionIconBindingLifecycle extends AbstractLifecycleProvider<ActionIconBinding, ActionIconBinding> {
        private final static String BEHAVIOR_ICON_MAP_DEP_KEY = "behaviorIconMap";
        private final static String ADMIN_ICON_MAP_DEP_KEY = "adminIconMap";
        
        public ActionIconBindingLifecycle(String behaviorIconMapId, String adminIconMapId) {
            super(new DescriptorListBuilder()
                    .dependency(BEHAVIOR_ICON_MAP_DEP_KEY, ImageIconMap.class)
                        .with(ImageIconMap.PROP_IMAGE_ICON_MAP_ID, behaviorIconMapId)
                    .dependency(ADMIN_ICON_MAP_DEP_KEY, ImageIconMap.class)
                        .with(ImageIconMap.PROP_IMAGE_ICON_MAP_ID, adminIconMapId)
                    .getDescriptors());
        }

        @Override
        protected ActionIconBinding create(Map<String, Object> dependencies) {
            return new ActionIconBinding(
                    (ImageIconMap) dependencies.get(BEHAVIOR_ICON_MAP_DEP_KEY),
                    (ImageIconMap) dependencies.get(ADMIN_ICON_MAP_DEP_KEY));
        }

        @Override
        protected void handleChange(String string, Object o, Map<String, Object> map) {
        }

        @Override
        protected Class<ActionIconBinding> getServiceClass() {
            return ActionIconBinding.class;
        }
    }
    
    public static void launchActionIconBindingLifecycle(BundleContext context, String behaviorPanelId, String adminPanelId) {
        new OSGiComponent(context, new ActionIconBinding.ActionIconBindingLifecycle(behaviorPanelId, adminPanelId)).start();
    }
        
}