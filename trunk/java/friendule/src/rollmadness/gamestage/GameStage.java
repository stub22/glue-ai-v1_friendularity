package rollmadness.gamestage;

import java.util.LinkedList;
import java.util.List;

public abstract class GameStage {

    private final GameStageEnvironment env;
    private GameStage parent;
    private final List<GameStage> children = new LinkedList<GameStage>();
    private final String name;

    public GameStage(GameStageEnvironment env, String name) {
	this.env = env;
	this.name = name;
    }

    public GameStageEnvironment getGameStageEnvironment() {
	return env;
    }

    public abstract void start();

    public abstract void pause();

    public abstract void stop();

    public void addChild(GameStage stage) {
	stage.parent = this;
	children.add(stage);
    }

    public void goBack() {
	if (parent != null) {
	    jumpTo(parent.name);
	}
    }

    public GameStage findStage(String stageName) {
	GameStage root = this;
	while (root.parent != null) {
	    root = root.parent;
	}
	LinkedList<GameStage> list = new LinkedList<GameStage>();
	list.add(root);
	while (!list.isEmpty()) {
	    GameStage s = list.pop();
	    if (s.name.equals(stageName)) {
		return s;
	    } else {
		list.addAll(s.children);
	    }
	}
	return null;
    }

    public void jumpTo(String stageName) {
	if (name.equals(stageName)) {
	    start();
	} else {
	    GameStage stage = findStage(stageName);
	    if (stage != null) {
		this.stop();
		stage.start();
	    }
	}
    }

    @Override
    public String toString() {
	return "GameStage [" + name + "]";
    }
}
