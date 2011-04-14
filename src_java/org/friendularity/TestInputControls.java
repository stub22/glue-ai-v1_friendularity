/* Copied from JMonkey code */

package org.friendularity;

import jme3test.input.*;
import com.jme3.app.SimpleApplication;
import com.jme3.input.KeyInput;
import com.jme3.input.MouseInput;
import com.jme3.input.controls.ActionListener;
import com.jme3.input.controls.AnalogListener;
import com.jme3.input.controls.KeyTrigger;
import com.jme3.input.controls.MouseAxisTrigger;

public class TestInputControls extends SimpleApplication {
    
    private ActionListener keyListener = new ActionListener(){
        public void onAction(String name, boolean pressed, float tpf){
            System.out.println("ACTION: " + name + " = " + pressed);
        }
    };
    public AnalogListener analogListener = new AnalogListener() {
        public void onAnalog(String name, float value, float tpf) {
            System.out.println("ANALOG: " + name + " = " + value);
        }
    };

    public static void main(String[] args){
        System.out.println("Starting TestControls app");
        TestInputControls app = new TestInputControls();
        app.start();
    }

    @Override
    public void simpleInitApp() {
        // Test multiple inputs per mapping
        inputManager.addMapping("key-space",
                new KeyTrigger(KeyInput.KEY_SPACE));
        inputManager.addMapping("axis-wheel",
                new MouseAxisTrigger(MouseInput.AXIS_WHEEL, false));
        inputManager.addMapping("axis-x",
                new MouseAxisTrigger(MouseInput.AXIS_X, false));
        // Test multiple listeners per mapping
        inputManager.addListener(keyListener, "key-space");
        inputManager.addListener(analogListener, "axis-wheel", "axis-x");
    }

}
