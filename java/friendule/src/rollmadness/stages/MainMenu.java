package rollmadness.stages;

import java.awt.Dimension;
import java.util.logging.Level;
import java.util.logging.Logger;
import jme3ui.event.action.UIAction;
import jme3ui.event.action.UIActionListener;
import jme3ui.layouts.UIGridLayout;
import jme3ui.widgets.UIButton;
import jme3ui.widgets.UIFrame;
import jme3ui.widgets.UIPanel;
import rollmadness.formats.TrackInfo;
import rollmadness.formats.TrackInfoSequence;
import rollmadness.gameproperties.PropertyKey;
import rollmadness.gamestage.GameStage;
import rollmadness.gamestage.GameStageEnvironment;

public class MainMenu extends GameStage {
    private final UIFrame FRAME;
    
    public MainMenu(GameStageEnvironment env) {
	super(env, MainMenu.class.getName());
	final UIButton newGameButton = new UIButton("New Game");
	final UIButton settingsButton = new UIButton("Settings");
	final UIButton exitButton = new UIButton("Exit");

	UIActionListener listener = new UIActionListener() {

	    public void onUIAction(UIAction e) {
		if(e.getSource() == newGameButton) {
		    startGame();
		} else if(e.getSource() == settingsButton) {
		    jumpTo(Settings.class.getName());
		} else if(e.getSource() == exitButton) {
		    Logger.getLogger(getClass().getName()).log(Level.INFO, "implement this");
		}
	    }
	};
	newGameButton.addUIActionListener(listener);
	settingsButton.addUIActionListener(listener);
	exitButton.addUIActionListener(listener);

	UIPanel panel = new UIPanel();
	panel.setLayout(new UIGridLayout(3, 1));
	panel.add(newGameButton);
	panel.add(settingsButton);
	panel.add(exitButton);
	
	UIFrame frame = new UIFrame();
	frame.setContents(panel);
	FRAME = frame;
    }

    private void startGame() {
	TrackInfoSequence seq = getGameStageEnvironment().
		getGlobalProperty(PropertyKey.TRACK_INFO_SEQUENCE,
		TrackInfoSequence.class);
	TrackInfo firstTrack = seq.next(null);
	getGameStageEnvironment().setGlobalProperty(PropertyKey.NEXT_TRACK_INFO,
		firstTrack);
	jumpTo(GameStart.class.getName());
    }

    @Override
    public void start() {
	getGameStageEnvironment().getInputManager().setCursorVisible(true);
	FRAME.resizeAndCenter(new Dimension(240, 200),
		getGameStageEnvironment().getScreenSize());
	FRAME.setVisible(true);
    }

    @Override
    public void pause() {
	
    }

    @Override
    public void stop() {
	FRAME.setVisible(false);
	getGameStageEnvironment().getInputManager().setCursorVisible(false);
    }
}
