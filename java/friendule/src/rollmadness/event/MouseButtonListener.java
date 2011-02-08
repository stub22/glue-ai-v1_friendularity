package rollmadness.event;

import com.jme3.input.RawInputListener;
import com.jme3.input.event.JoyAxisEvent;
import com.jme3.input.event.JoyButtonEvent;
import com.jme3.input.event.KeyInputEvent;
import com.jme3.input.event.MouseButtonEvent;
import com.jme3.input.event.MouseMotionEvent;
import java.util.HashMap;
import java.util.Map;

public abstract class MouseButtonListener implements RawInputListener {
    private int x, y;
    private Map<Integer, Boolean> pressedMap = new HashMap<Integer, Boolean>();

    public void onJoyAxisEvent(JoyAxisEvent evt) {
    }

    public void onJoyButtonEvent(JoyButtonEvent evt) {
    }

    public void onMouseMotionEvent(MouseMotionEvent e) {
	x = e.getX();
	y = e.getY();
    }

    public void onMouseButtonEvent(MouseButtonEvent e) {
	if(e.isPressed()) {
	    if(!wasPressed(e.getButtonIndex())) {
		setPressed(e.getButtonIndex());
		mousePressed(e.getButtonIndex(), x, y);
	    }
	} else if(e.isReleased()) {
	    setReleased(e.getButtonIndex());
	    mouseReleased(e.getButtonIndex(), x, y);
	}
    }

    public void onKeyEvent(KeyInputEvent evt) {
    }

    public abstract void mousePressed(int button, int x, int y);
    public abstract void mouseReleased(int button, int x, int y);

    private boolean wasPressed(int index) {
	Boolean value = pressedMap.get(index);
	if(value == null) pressedMap.put(index, value = false);
	return value;
    }

    private void setPressed(int index) {
	pressedMap.put(index, true);
    }

    private void setReleased(int index) {
	pressedMap.put(index, false);
    }
}
