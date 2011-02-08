package rollmadness.logic.reactions;

import jme3clogic.Reaction;
import jme3clogic.basic.NumericValueHolder;

public class SetNumericField extends Reaction {
    private final NumericValueHolder target;
    private final Number value;
    private final String field;

    public SetNumericField(NumericValueHolder target, String field, Number value) {
	this.target = target;
	this.value = value;
	this.field = field;
    }

    @Override
    public void act(float timePerFrame) {
	target.set(field, value);
    }

}
