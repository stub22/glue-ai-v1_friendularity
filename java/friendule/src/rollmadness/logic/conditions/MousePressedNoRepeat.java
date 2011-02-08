package rollmadness.logic.conditions;

import com.jme3.input.InputManager;
import com.jme3.input.RawInputListener;
import com.jme3.input.event.JoyAxisEvent;
import com.jme3.input.event.JoyButtonEvent;
import com.jme3.input.event.KeyInputEvent;
import com.jme3.input.event.MouseButtonEvent;
import com.jme3.input.event.MouseMotionEvent;
import jme3clogic.Condition;

public final class MousePressedNoRepeat extends Condition implements RawInputListener {
    private final InputManager inputManager;
    private final int button;
    private boolean verified;
    private boolean wasPressed;
    
    public MousePressedNoRepeat(InputManager im, int mousebutton) {
	this.inputManager = im;
	this.button = mousebutton;
	im.addRawInputListener(this);
    }

    @Override
    public void dispose() {
	inputManager.removeRawInputListener(this);
    }

    @Override
    public boolean holds(float timePerFrame) {
	if(verified) {
	    verified = false;
	    return true;
	} else {
	    return false;
	}
    }

    public void onJoyAxisEvent(JoyAxisEvent evt) {

    }

    public void onJoyButtonEvent(JoyButtonEvent evt) {

    }

    public void onMouseMotionEvent(MouseMotionEvent evt) {

    }

    public void onMouseButtonEvent(MouseButtonEvent e) {
	if(e.getButtonIndex() == button) {
	    if(e.isPressed() && !wasPressed) {
		wasPressed = verified = true;
	    } else if(e.isReleased()) {
		wasPressed = false;
	    }
	}
    }

    public void onKeyEvent(KeyInputEvent evt) {

    }

}
