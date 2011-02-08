package rollmadness.logic.conditions;

import com.jme3.input.InputManager;
import com.jme3.input.RawInputListener;
import com.jme3.input.event.JoyAxisEvent;
import com.jme3.input.event.JoyButtonEvent;
import com.jme3.input.event.KeyInputEvent;
import com.jme3.input.event.MouseButtonEvent;
import com.jme3.input.event.MouseMotionEvent;
import jme3clogic.Condition;

public final class MouseMoving extends Condition implements RawInputListener {
    private final InputManager inputManager;
    private final int threshold;
    private boolean conditionState;

    public MouseMoving(InputManager im, int threshold) {
	inputManager = im;
	this.threshold = threshold;
	inputManager.addRawInputListener(this);
    }

    @Override
    public void dispose() {
	inputManager.removeRawInputListener(this);
    }

    @Override
    public boolean holds(float timePerFrame) {
	if(conditionState) {
	    conditionState = false;
	    return true;
	} else {
	    return false;
	}
    }

    public void onJoyAxisEvent(JoyAxisEvent evt) {

    }

    public void onJoyButtonEvent(JoyButtonEvent evt) {

    }

    public void onMouseMotionEvent(MouseMotionEvent e) {
	conditionState = e.getDX() > threshold || e.getDY() > threshold;
    }

    public void onMouseButtonEvent(MouseButtonEvent evt) {

    }

    public void onKeyEvent(KeyInputEvent evt) {

    }

}
