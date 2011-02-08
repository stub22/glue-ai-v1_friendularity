package rollmadness.util;

import com.jme3.input.FlyByCamera;
import com.jme3.input.InputManager;
import com.jme3.input.KeyInput;
import com.jme3.input.controls.ActionListener;
import com.jme3.input.controls.AnalogListener;
import com.jme3.input.controls.KeyTrigger;
import com.jme3.input.controls.MouseAxisTrigger;
import com.jme3.math.Matrix3f;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.renderer.Camera;

/**
 * A first person view camera controller.
 * After creation, you must register the camera controller with the
 * dispatcher using #registerWithDispatcher().
 *
 * Controls:
 *  - Move the mouse to rotate the camera
 *  - Mouse wheel for zooming in or out
 *  - WASD keys for moving forward/backward and strafing
 *  - QZ keys raise or lower the camera
 */
public class MouseCamera implements AnalogListener, ActionListener {

    private Camera cam;
    private float rotationSpeed = 1f;
    private boolean enabled = true;
    private InputManager inputManager;
    private final Vector3f UP;

    public MouseCamera(Camera cam, InputManager im){
        this.cam = cam;
	UP = new Vector3f(cam.getUp());
	registerWithInput(im);
    }


    public void setEnabled(boolean enable){
        enabled = enable;
    }

    /**
     * @return If enabled
     * @see FlyByCamera#setEnabled(boolean)
     */
    public boolean isEnabled(){
        return enabled;
    }


    /**
     * Registers the FlyByCamera to recieve input events from the provided
     * Dispatcher.
     * @param dispacher
     */
    private void registerWithInput(InputManager inputManager){
        this.inputManager = inputManager;
        String[] mappings = new String[]{
            "MouseCamera_Left",
            "MouseCamera_Right",
            "MouseCamera_Up",
            "MouseCamera_Down"
        };

        // both mouse and button - rotation of cam
        inputManager.addMapping("MouseCamera_Left", new MouseAxisTrigger(0, true),
                                               new KeyTrigger(KeyInput.KEY_LEFT));

        inputManager.addMapping("MouseCamera_Right", new MouseAxisTrigger(0, false),
                                                new KeyTrigger(KeyInput.KEY_RIGHT));

        inputManager.addMapping("MouseCamera_Up", new MouseAxisTrigger(1, false),
                                             new KeyTrigger(KeyInput.KEY_UP));

        inputManager.addMapping("MouseCamera_Down", new MouseAxisTrigger(1, true),
                                               new KeyTrigger(KeyInput.KEY_DOWN));


        inputManager.addListener(this, mappings);
    }

    private void rotateCamera(float value, Vector3f axis){

        Matrix3f mat = new Matrix3f();
        mat.fromAngleNormalAxis(rotationSpeed * value, axis);

        Vector3f up = cam.getUp();
        Vector3f left = cam.getLeft();
        Vector3f dir = cam.getDirection();

        mat.mult(up, up);
        mat.mult(left, left);
        mat.mult(dir, dir);

        Quaternion q = new Quaternion();
        q.fromAxes(left, up, dir);
        q.normalize();

        cam.setAxes(q);
    }

    public void onAnalog(String name, float value, float tpf) {
        if (!enabled)
            return;

        if (name.equals("MouseCamera_Left")){
            rotateCamera(value, UP);
        }else if (name.equals("MouseCamera_Right")){
            rotateCamera(-value, UP);
        }else if (name.equals("MouseCamera_Up")){
            rotateCamera(-value, cam.getLeft());
        }else if (name.equals("MouseCamera_Down")){
            rotateCamera(value, cam.getLeft());
        }
    }

    public void onAction(String name, boolean value, float tpf) {
    }

}
