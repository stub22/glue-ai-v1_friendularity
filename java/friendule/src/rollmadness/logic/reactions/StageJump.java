package rollmadness.logic.reactions;

import jme3clogic.Reaction;
import rollmadness.gamestage.GameStage;

public class StageJump extends Reaction {
    private final GameStage source;
    private final String destName;

    public StageJump(GameStage source, String destName) {
	this.source = source;
	this.destName = destName;
    }

    @Override
    public void act(float timePerFrame) {
	source.jumpTo(destName);
    }

}
