package rollmadness.stages;

import java.awt.Dimension;
import jme3ui.event.action.UIAction;
import jme3ui.event.action.UIActionListener;
import jme3ui.layouts.UIGridLayout;
import jme3ui.widgets.UIButton;
import jme3ui.widgets.UIFrame;
import jme3ui.widgets.UIPanel;
import rollmadness.formats.TrackInfo;
import rollmadness.gameproperties.PropertyKey;
import rollmadness.gamestage.GameStage;
import rollmadness.gamestage.GameStageEnvironment;

public class GameOver extends GameStage {
    private final UIFrame FRAME;

    public GameOver(GameStageEnvironment env) {
	super(env, GameOver.class.getName());
	UIPanel panel = new UIPanel();
	panel.setLayout(new UIGridLayout(2, 1));
	UIButton tryAgain = new UIButton("Try Again");
	UIButton mainMenu = new UIButton("Main Menu");
	tryAgain.addUIActionListener(new UIActionListener() {

	    public void onUIAction(UIAction e) {
		TrackInfo lastTrack = getGameStageEnvironment().
			getGlobalProperty(PropertyKey.LAST_TRACK_INFO,
			TrackInfo.class);
		getGameStageEnvironment().setGlobalProperty(
			PropertyKey.NEXT_TRACK_INFO, lastTrack);
		jumpTo(GameStart.class.getName());
	    }
	});
	mainMenu.addUIActionListener(new UIActionListener() {

	    public void onUIAction(UIAction e) {
		jumpTo(MainMenu.class.getName());
	    }
	});
	panel.add(tryAgain);
	panel.add(mainMenu);
	UIFrame frame = new UIFrame();
	frame.setContents(panel);
	FRAME = frame;
    }

    @Override
    public void start() {
	getGameStageEnvironment().getInputManager().setCursorVisible(true);
	Dimension screenSize = getGameStageEnvironment().getScreenSize();
	FRAME.resizeAndCenter(new Dimension(400, 200), screenSize);
	FRAME.setVisible(true);
    }

    @Override
    public void pause() {
    }

    @Override
    public void stop() {
	getGameStageEnvironment().getInputManager().setCursorVisible(false);
	FRAME.setVisible(false);
    }

}
