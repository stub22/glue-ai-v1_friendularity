package rollmadness.logic.conditions;

import com.jme3.input.InputManager;
import com.jme3.input.RawInputListener;
import com.jme3.input.event.JoyAxisEvent;
import com.jme3.input.event.JoyButtonEvent;
import com.jme3.input.event.KeyInputEvent;
import com.jme3.input.event.MouseButtonEvent;
import com.jme3.input.event.MouseMotionEvent;
import jme3clogic.Condition;

public final class KeyPressedNoRepeat extends Condition implements RawInputListener {
    private final InputManager input;
    private final int keycode;
    private boolean verified;
    private boolean wasPressed;

    public KeyPressedNoRepeat(InputManager im, int keycode) {
	this.input = im;
	this.keycode = keycode;
	input.addRawInputListener(this);
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

    @Override
    public void dispose() {
	input.removeRawInputListener(this);
    }

    public void onJoyAxisEvent(JoyAxisEvent evt) {

    }

    public void onJoyButtonEvent(JoyButtonEvent evt) {

    }

    public void onMouseMotionEvent(MouseMotionEvent evt) {

    }

    public void onMouseButtonEvent(MouseButtonEvent evt) {

    }

    public void onKeyEvent(KeyInputEvent e) {
	if(e.getKeyCode() == keycode) {
	    if(e.isPressed() && !wasPressed) {
		wasPressed = verified = true;
	    } else if(e.isReleased()) {
		wasPressed = false;
	    }
	}
    }
}
